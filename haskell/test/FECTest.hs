{-# LANGUAGE DerivingStrategies #-}

module Main where

import Test.Hspec

import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List (sortOn)
import Data.Serializer
import Data.Word

import System.IO (IOMode (..), withFile)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- Imported for the orphan Arbitrary ByteString instance.
import Test.QuickCheck.Instances.ByteString ()

-- | Valid ZFEC parameters.
data Params = Params
    { required :: Int -- aka k
    , total :: Int -- aka n
    }
    deriving (Show, Ord, Eq)

-- | A somewhat efficient generator for valid ZFEC parameters.
instance Arbitrary Params where
    arbitrary = do
        required <- choose (1, 255)
        total <- choose (required, 255)
        return $ Params required total

instance Arbitrary FEC.FECParams where
    arbitrary = do
      (Params required total) <- arbitrary :: Gen Params
      return $ FEC.fec required total

randomTake :: Int -> Int -> [a] -> [a]
randomTake seed n values = map snd $ take n sortedValues
  where
    sortedValues = sortOn fst taggedValues
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

-- | @FEC.secureDivide@ is the inverse of @FEC.secureCombine@.
prop_divide :: Word16 -> Word8 -> Word8 -> Property
prop_divide size byte divisor = monadicIO $ do
  let input = B.replicate (fromIntegral size + 1) byte
  parts <- run $ FEC.secureDivide (fromIntegral divisor) input
  assert (FEC.secureCombine parts == input)

-- | @FEC.encode@ is the inverse of @FEC.decode@.
prop_decode :: FEC.FECParams -> Word16 -> Int -> Property
prop_decode fec len seed = property $ testFEC fec len seed

-- | @FEC.enFEC@ is the inverse of @FEC.deFEC@.
prop_deFEC :: Params -> B.ByteString -> Property
prop_deFEC (Params required total) testdata =
  FEC.deFEC required total minimalShares === testdata
  where
    allShares = FEC.enFEC required total testdata
    minimalShares = take required allShares

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
        it "works with required=255" $ property $ prop_decode (FEC.fec 255 255)
