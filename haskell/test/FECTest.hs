{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.Hspec

import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import Data.List (sortBy)
import System.IO (IOMode (..), withFile)
import System.Random
import Data.Int
import Data.Serializer
import Test.QuickCheck
import Test.QuickCheck.Monadic

newtype ArbByteString = ArbByteString BL.ByteString deriving newtype (Show, Ord, Eq)

instance Arbitrary ArbByteString where
  arbitrary = do
    len <- choose (0, 1024 * 64) :: Gen Int32
    -- Invent some bytes that are somewhat distinctive-ish.
    return . ArbByteString $ expand len (toLazyByteString len)

expand :: Integral i => i -> BL.ByteString -> BL.ByteString
expand len = BL.take (fromIntegral len) . BL.cycle

-- | Valid ZFEC parameters.
data Params = Params
    { required :: Int -- aka k
    , total :: Int -- aka n
    }
    deriving (Show, Ord, Eq)

-- | A somewhat efficient generator for valid ZFEC parameters.
instance Arbitrary Params where
    arbitrary = do
        required <- choose (1, 256)
        total <- choose (required, 256)
        return $ Params required total

instance Arbitrary FEC.FECParams where
    arbitrary = do
      (Params required total) <- arbitrary :: Gen Params
      return $ FEC.fec required total

randomTake :: Int -> Int -> [a] -> [a]
randomTake seed n values = map snd $ take n sortedValues
  where
    sortedValues = sortBy (\a b -> compare (fst a) (fst b)) taggedValues
    taggedValues = zip rnds values
    rnds :: [Float]
    rnds = randoms gen
    gen = mkStdGen seed

-- | Any combination of the inputs blocks and the output blocks from
-- @FEC.encode@, as long as there are at least @k@ of them, can be recombined
-- using @FEC.decode@ to produce the original input blocks.
--
testFEC
  :: FEC.FECParams
  -- ^ The FEC parameters to exercise.
  -> Word16
  -- ^ The length of the blocks to exercise.
  -> Int
  -- ^ A random seed to use to be able to vary the choice of which blocks to
  -- try to decode.
  -> Bool
  -- ^ True if the encoded input was reconstructed by decoding, False
  -- otherwise.
testFEC fec len seed = FEC.decode fec someTaggedBlocks == origBlocks
  where
    -- Construct some blocks.  Each will just be the byte corresponding to the
    -- block number repeated to satisfy the requested length.
    origBlocks = B.replicate (fromIntegral len) . fromIntegral <$> [0 .. (FEC.paramK fec - 1)]

    -- Encode the data to produce the "secondary" blocks which (might) add
    -- redundancy to the original blocks.
    secondaryBlocks = FEC.encode fec origBlocks

    -- Tag each block with its block number because the decode API requires
    -- this information.
    taggedBlocks = zip [0 ..] (origBlocks ++ secondaryBlocks)

    -- Choose enough of the tagged blocks (some combination of original and
    -- secondary) to try to use for decoding.
    someTaggedBlocks = randomTake seed (FEC.paramK fec) taggedBlocks

prop_divide :: Word16 -> Word8 -> Word8 -> Property
prop_divide size byte divisor = monadicIO $ do
  let input = B.replicate (fromIntegral size + 1) byte
  parts <- run $ FEC.secureDivide (fromIntegral divisor) input
  assert (FEC.secureCombine parts == input)

prop_decode :: FEC.FECParams -> Word16 -> Int -> Property
prop_decode fec len seed = len < 1024 ==> testFEC fec len seed

prop_deFEC :: Params -> ArbByteString -> Property
prop_deFEC (Params required total) (ArbByteString testdata) =
  FEC.deFEC required total minimalShares === testdataStrict
  where
    allShares = FEC.enFEC required total testdataStrict
    minimalShares = take required allShares
    testdataStrict = BL.toStrict testdata

main :: IO ()
main = hspec $ do
    describe "secureCombine" $ do
        -- secureDivide is insanely slow and memory hungry for large inputs,
        -- like QuickCheck will find with it as currently defined.  Just pass
        -- some small inputs.  It's not clear it's worth fixing (or even
        -- keeping) thesefunctions.  They don't seem to be used by anything.
        -- Why are they here?
        it "is the inverse of secureDivide n" $ once $ prop_divide 1024 65 3

    describe "deFEC" $ do
        it "is the inverse of enFEC" $ (withMaxSuccess 2000 prop_deFEC)

    describe "decode" $ do
        it "is (nearly) the inverse of encode" $ (withMaxSuccess 2000 prop_decode)
        it "works with total=256" $ property $ prop_decode (FEC.fec 1 256)
        it "works with required=256" $ property $ prop_decode (FEC.fec 256 256)
