{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-- |
-- Module:    Codec.FEC
-- Copyright: Adam Langley
-- License:   GPLv2+|TGPPLv1+ (see README.rst for details)
--
-- Stability: experimental
--
-- The module provides k of n encoding - a way to generate (n - k) secondary
-- blocks of data from k primary blocks such that any k blocks (primary or
-- secondary) are sufficient to regenerate all blocks.
--
-- All blocks must be the same length and you need to keep track of which
-- blocks you have in order to tell decode. By convention, the blocks are
-- numbered 0..(n - 1) and blocks numbered < k are the primary blocks.

module Codec.FEC (
    FECParams
  , fec
  , encode
  , decode

  -- * Utility functions
  , secureDivide
  , secureCombine
  , enFEC
  , deFEC
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Data.Bits (xor)
import Data.List (sortBy, partition, (\\), nub)
import Foreign.Ptr
import Foreign.Storable (sizeOf, poke)
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (withArray, advancePtr)
import System.IO (withFile, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)

data CFEC
data FECParams = FECParams (ForeignPtr CFEC) Int Int

instance Show FECParams where
  show (FECParams _ k n) = "FEC (" ++ show k ++ ", " ++ show n ++ ")"

foreign import ccall unsafe "fec_new" _new :: CUInt  -- ^ k
                                           -> CUInt  -- ^ n
                                           -> IO (Ptr CFEC)
foreign import ccall unsafe "&fec_free" _free :: FunPtr (Ptr CFEC -> IO ())
foreign import ccall unsafe "fec_encode" _encode :: Ptr CFEC
                                                 -> Ptr (Ptr Word8)  -- ^ primary blocks
                                                 -> Ptr (Ptr Word8)  -- ^ (output) secondary blocks
                                                 -> Ptr CUInt  -- ^ array of secondary block ids
                                                 -> CSize  -- ^ length of previous
                                                 -> CSize  -- ^ block length
                                                 -> IO ()
foreign import ccall unsafe "fec_decode" _decode :: Ptr CFEC
                                                 -> Ptr (Ptr Word8)  -- ^ input blocks
                                                 -> Ptr (Ptr Word8)  -- ^ output blocks
                                                 -> Ptr CUInt  -- ^ array of input indexes
                                                 -> CSize  -- ^ block length
                                                 -> IO ()

-- | Return true if the given @k@ and @n@ values are valid
isValidConfig :: Int -> Int -> Bool
isValidConfig k n
  | k >= n = False
  | k < 1 = False
  | n < 1 = False
  | n > 255 = False
  | otherwise = True

-- | Return a FEC with the given parameters.
fec :: Int  -- ^ the number of primary blocks
    -> Int  -- ^ the total number blocks, must be < 256
    -> FECParams
fec k n =
  if not (isValidConfig k n)
     then error $ "Invalid FEC parameters: " ++ show k ++ " " ++ show n
     else unsafePerformIO (do
       cfec <- _new (fromIntegral k) (fromIntegral n)
       params <- newForeignPtr _free cfec
       return $ FECParams params k n)

-- | Create a C array of unsigned from an input array
uintCArray :: [Int] -> ((Ptr CUInt) -> IO a) -> IO a
uintCArray xs f = withArray (map fromIntegral xs) f

-- | Convert a list of ByteStrings to an array of pointers to their data
byteStringsToArray :: [B.ByteString] -> ((Ptr (Ptr Word8)) -> IO a) -> IO a
byteStringsToArray inputs f = do
  let l = length inputs
  allocaBytes (l * sizeOf (undefined :: Ptr Word8)) (\array -> do
    let inner _ [] = f array
        inner array' (bs : bss) = BU.unsafeUseAsCString bs (\ptr -> do
          poke array' $ castPtr ptr
          inner (advancePtr array' 1) bss)
    inner array inputs)

-- | Return True iff all the given ByteStrings are the same length
allByteStringsSameLength :: [B.ByteString] -> Bool
allByteStringsSameLength [] = True
allByteStringsSameLength (bs : bss) = all ((==) (B.length bs)) $ map B.length bss

-- | Run the given function with a pointer to an array of @n@ pointers to
--   buffers of size @size@. Return these buffers as a list of ByteStrings
createByteStringArray :: Int  -- ^ the number of buffers requested
                      -> Int  -- ^ the size of each buffer
                      -> ((Ptr (Ptr Word8)) -> IO ())
                      -> IO [B.ByteString]
createByteStringArray n size f = do
  allocaBytes (n * sizeOf (undefined :: Ptr Word8)) (\array -> do
    allocaBytes (n * size) (\ptr -> do
      mapM_ (\i -> poke (advancePtr array i) (advancePtr ptr (size * i))) [0..(n - 1)]
      f array
      mapM (\i -> B.packCStringLen (castPtr $ advancePtr ptr (i * size), size)) [0..(n - 1)]))

-- | Generate the secondary blocks from a list of the primary blocks. The
--   primary blocks must be in order and all of the same size. There must be
--   @k@ primary blocks.
encode :: FECParams
       -> [B.ByteString]  -- ^ a list of @k@ input blocks
       -> [B.ByteString]  -- ^ (n - k) output blocks
encode (FECParams params k n) inblocks
  | length inblocks /= k = error "Wrong number of blocks to FEC encode"
  | not (allByteStringsSameLength inblocks) = error "Not all inputs to FEC encode are the same length"
  | otherwise = unsafePerformIO (do
      let sz = B.length $ head inblocks
      withForeignPtr params (\cfec -> do
        byteStringsToArray inblocks (\src -> do
          createByteStringArray (n - k) sz (\fecs -> do
            uintCArray [k..(n - 1)] (\block_nums -> do
              _encode cfec src fecs block_nums (fromIntegral (n - k)) $ fromIntegral sz)))))

-- | A sort function for tagged assoc lists
sortTagged :: [(Int, a)] -> [(Int, a)]
sortTagged = sortBy (\a b -> compare (fst a) (fst b))

-- | Reorder the given list so that elements with tag numbers < the first
--   argument have an index equal to their tag number (if possible)
reorderPrimaryBlocks :: Int -> [(Int, a)] -> [(Int, a)]
reorderPrimaryBlocks n blocks = inner (sortTagged pBlocks) sBlocks [] where
  (pBlocks, sBlocks) = partition (\(tag, _) -> tag < n) blocks
  inner [] sBlocks acc = acc ++ sBlocks
  inner pBlocks [] acc = acc ++ pBlocks
  inner pBlocks@((tag, a) : ps) sBlocks@(s : ss) acc =
    if length acc == tag
       then inner ps sBlocks (acc ++ [(tag, a)])
       else inner pBlocks ss (acc ++ [s])

-- | Recover the primary blocks from a list of @k@ blocks. Each block must be
--   tagged with its number (see the module comments about block numbering)
decode :: FECParams
       -> [(Int, B.ByteString)]  -- ^ a list of @k@ blocks and their index
       -> [B.ByteString]  -- ^ a list the @k@ primary blocks
decode (FECParams params k n) inblocks
  | length (nub $ map fst inblocks) /= length (inblocks) = error "Duplicate input blocks in FEC decode"
  | any (\f -> f < 0 || f >= n) $ map fst inblocks = error "Invalid block numbers in FEC decode"
  | length inblocks /= k = error "Wrong number of blocks to FEC decode"
  | not (allByteStringsSameLength $ map snd inblocks) = error "Not all inputs to FEC decode are same length"
  | otherwise = unsafePerformIO (do
      let sz = B.length $ snd $ head inblocks
          inblocks' = reorderPrimaryBlocks k inblocks
          presentBlocks = map fst inblocks'
      withForeignPtr params (\cfec -> do
        byteStringsToArray (map snd inblocks') (\src -> do
          b <- createByteStringArray (n - k) sz (\out -> do
                 uintCArray presentBlocks (\block_nums -> do
                   _decode cfec src out block_nums $ fromIntegral sz))
          let blocks = [0..(n - 1)] \\ presentBlocks
              tagged = zip blocks b
              allBlocks = sortTagged $ tagged ++ inblocks'
          return $ take k $ map snd allBlocks)))

-- | Break a ByteString into @n@ parts, equal in length to the original, such
--   that all @n@ are required to reconstruct the original, but having less
--   than @n@ parts reveals no information about the orginal.
--
--   This code works in IO monad because it needs a source of random bytes,
--   which it gets from /dev/urandom. If this file doesn't exist an
--   exception results
--
--   Not terribly fast - probably best to do it with short inputs (e.g. an
--   encryption key)
secureDivide :: Int  -- ^ the number of parts requested
             -> B.ByteString  -- ^ the data to be split
             -> IO [B.ByteString]
secureDivide n input
  | n < 0 = error "secureDivide called with negative number of parts"
  | otherwise = withFile "/dev/urandom" ReadMode (\handle -> do
      let inner 1 bs = return [bs]
          inner n bs = do
            mask <- B.hGet handle (B.length bs)
            let masked = B.pack $ B.zipWith xor bs mask
            rest <- inner (n - 1) masked
            return (mask : rest)
      inner n input)

-- | Reverse the operation of secureDivide. The order of the inputs doesn't
--   matter, but they must all be the same length
secureCombine :: [B.ByteString] -> B.ByteString
secureCombine [] = error "Passed empty list of inputs to secureCombine"
secureCombine [a] = a
secureCombine [a, b] = B.pack $ B.zipWith xor a b
secureCombine (a : rest) = B.pack $ B.zipWith xor a $ secureCombine rest

-- | A utility function which takes an arbitary input and FEC encodes it into a
--   number of blocks. The order the resulting blocks doesn't matter so long
--   as you have enough to present to @deFEC@.
enFEC :: Int  -- ^ the number of blocks required to reconstruct
      -> Int  -- ^ the total number of blocks
      -> B.ByteString  -- ^ the data to divide
      -> [B.ByteString]  -- ^ the resulting blocks
enFEC k n input = taggedPrimaryBlocks ++ taggedSecondaryBlocks where
  taggedPrimaryBlocks = map (uncurry B.cons) $ zip [0..] primaryBlocks
  taggedSecondaryBlocks = map (uncurry B.cons) $ zip [(fromIntegral k)..] secondaryBlocks
  remainder = B.length input `mod` k
  paddingLength = if remainder >= 1 then (k - remainder) else k
  paddingBytes = (B.replicate (paddingLength - 1) 0) `B.append` (B.singleton $ fromIntegral paddingLength)
  divide a bs
    | B.null bs = []
    | otherwise = (B.take a bs) : (divide a $ B.drop a bs)
  input' = input `B.append` paddingBytes
  blockSize = B.length input' `div` k
  primaryBlocks = divide blockSize input'
  secondaryBlocks = encode params primaryBlocks
  params = fec k n

-- | Reverses the operation of @enFEC@.
deFEC :: Int  -- ^ the number of blocks required (matches call to @enFEC@)
      -> Int  -- ^ the total number of blocks (matches call to @enFEC@)
      -> [B.ByteString]  -- ^ a list of k, or more, blocks from @enFEC@
      -> B.ByteString
deFEC k n inputs
  | length inputs < k = error "Too few inputs to deFEC"
  | otherwise = B.take (B.length fecOutput - paddingLength) fecOutput where
      paddingLength = fromIntegral $ B.last fecOutput
      inputs' = take k inputs
      taggedInputs = map (\bs -> (fromIntegral $ B.head bs, B.tail bs)) inputs'
      fecOutput = B.concat $ decode params taggedInputs
      params = fec k n
