{-# LANGUAGE DerivingStrategies #-}

module Main where

import Test.Hspec (describe, hspec, it, parallel)

import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Word (Word16, Word8)
import System.Random (Random (randoms), mkStdGen)
import Test.QuickCheck (
    Arbitrary (arbitrary),
    Property,
    Testable (property),
    choose,
    conjoin,
    once,
    withMaxSuccess,
    (===),
 )
import Test.QuickCheck.Monadic (assert, monadicIO, run)

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
    arbitrary =
        choose (1, 255)
            >>= \req -> Params req <$> choose (req, 255)

randomTake :: Int -> Int -> [a] -> [a]
randomTake seed n values = map snd $ take n sortedValues
  where
    sortedValues = sortOn fst taggedValues
    taggedValues = zip rnds values
    rnds :: [Float]
    rnds = randoms gen
    gen = mkStdGen seed

{- | Any combination of the inputs blocks and the output blocks from
 @FEC.encode@, as long as there are at least @k@ of them, can be recombined
 using @FEC.decode@ to produce the original input blocks.
-}
testFEC ::
    -- | The FEC parameters to exercise.
    FEC.FECParams ->
    -- | The length of the blocks to exercise.
    Word16 ->
    -- | A random seed to use to be able to vary the choice of which blocks to
    -- try to decode.
    Int ->
    -- | True if the encoded input was reconstructed by decoding, False
    -- otherwise.
    Bool
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
prop_decode :: Params -> Word16 -> Int -> Property
prop_decode (Params req tot) len seed = property $ do
    testFEC fec len seed === True
  where
    fec = FEC.fec req tot

-- | @FEC.enFEC@ is the inverse of @FEC.deFEC@.
prop_deFEC :: Params -> B.ByteString -> Property
prop_deFEC (Params req tot) testdata =
    FEC.deFEC req tot minimalShares === testdata
  where
    allShares = FEC.enFEC req tot testdata
    minimalShares = take req allShares

prop_primary_copies :: Params -> BL.ByteString -> Property
prop_primary_copies (Params _ tot) primary = property $ do
    conjoin $ (BL.toStrict primary ===) <$> secondary
  where
    fec = FEC.fec 1 tot
    secondary = FEC.encode fec [BL.toStrict primary]

main :: IO ()
main = hspec $
    parallel $ do
        describe "secureCombine" $ do
            -- secureDivide is insanely slow and memory hungry for large inputs,
            -- like QuickCheck will find with it as currently defined.  Just pass
            -- some small inputs.  It's not clear it's worth fixing (or even
            -- keeping) thesefunctions.  They don't seem to be used by anything.
            -- Why are they here?
            it "is the inverse of secureDivide n" $ once $ prop_divide 1024 65 3

        describe "deFEC" $ do
            it "is the inverse of enFEC" (withMaxSuccess 2000 prop_deFEC)

        describe "decode" $ do
            it "is (nearly) the inverse of encode" (withMaxSuccess 2000 prop_decode)
            it "works with required=255" $ property $ prop_decode (Params 255 255)

        describe "encode" $ do
            it "returns copies of the primary block for all 1 of N encodings" $ property $ withMaxSuccess 10000 prop_primary_copies
