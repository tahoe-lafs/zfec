{-# LANGUAGE DerivingStrategies #-}


module Main where

import Test.Hspec (describe, hspec, it, parallel, Expectation, shouldBe)

import Control.Monad (replicateM_)

import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import Data.List (sortOn)
import Data.Word (Word16, Word8)
import System.Random (Random (randoms), mkStdGen)
import Test.QuickCheck ( Arbitrary (arbitrary), Property, Testable (property), choose, once, withMaxSuccess)
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
    arbitrary = do
        req <- choose (1, 255)
        tot <- choose (req, 255)
        return $ Params req tot

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
    Expectation
testFEC fec len seed = do
    -- Encode the data to produce the "secondary" blocks which (might) add
    -- redundancy to the original blocks.
    secondaryBlocks <- FEC.encode fec origBlocks

    let -- Tag each block with its block number because the decode API requires
        -- this information.
        taggedBlocks = zip [0 ..] (origBlocks ++ secondaryBlocks)

        -- Choose enough of the tagged blocks (some combination of original and
        -- secondary) to try to use for decoding.
        someTaggedBlocks = randomTake seed (FEC.paramK fec) taggedBlocks

    decoded <- FEC.decode fec someTaggedBlocks
    decoded `shouldBe` origBlocks
  where
    -- Construct some blocks.  Each will just be the byte corresponding to the
    -- block number repeated to satisfy the requested length.
    origBlocks = B.replicate (fromIntegral len) . fromIntegral <$> [0 .. (FEC.paramK fec - 1)]

-- | @FEC.secureDivide@ is the inverse of @FEC.secureCombine@.
prop_divide :: Word16 -> Word8 -> Word8 -> Property
prop_divide size byte divisor = monadicIO $ do
    let input = B.replicate (fromIntegral size + 1) byte
    parts <- run $ FEC.secureDivide (fromIntegral divisor) input
    assert (FEC.secureCombine parts == input)

-- | @FEC.encode@ is the inverse of @FEC.decode@.
prop_decode :: Params -> Word16 -> Int -> Property
prop_decode (Params req tot) len seed =
    monadicIO . run $ do
        fec <- FEC.fec req tot
        testFEC fec len seed

-- | @FEC.enFEC@ is the inverse of @FEC.deFEC@.
prop_deFEC :: Params -> B.ByteString -> Property
prop_deFEC (Params req tot) testdata = monadicIO $ do
    encoded <- run $ FEC.enFEC req tot testdata
    decoded <- run $ FEC.deFEC req tot (take req encoded)
    assert $ testdata == decoded

prop_primary_copies :: Params -> B.ByteString -> Property
prop_primary_copies (Params _ tot) primary = monadicIO $ do
    fec <- run $ FEC.fec 1 tot
    secondary <- run $ FEC.encode fec [primary]
    assert $ all (primary ==) secondary

main :: IO ()
main = do
    -- Be sure to do the required zfec initialization first.
    hspec . parallel $ do
        describe "encode" $ do
            -- This test originally caught a bug in multi-threaded
            -- initialization of the C library.  Since it is in the
            -- initialization codepath, it cannot catch the bug if it runs
            -- after initialization has happened.  So we put it first in the
            -- suite and hope that nothing manages to get in before this.
            --
            -- Since the bug has to do with multi-threaded use, we also make a
            -- lot of copies of this property so hspec can run them in parallel
            -- (QuickCheck will not do anything in parallel inside a single
            -- property).
            --
            -- Still, there's non-determinism and the behavior is only revealed
            -- by this property sometimes.
            replicateM_ 20 $
                it "returns copies of the primary block for all 1 of N encodings" $
                    withMaxSuccess 5 prop_primary_copies

        describe "secureCombine" $ do
            -- secureDivide is insanely slow and memory hungry for large inputs,
            -- like QuickCheck will find with it as currently defined.  Just pass
            -- some small inputs.  It's not clear it's worth fixing (or even
            -- keeping) thesefunctions.  They don't seem to be used by anything.
            -- Why are they here?
            it "is the inverse of secureDivide n" $ once $ prop_divide 1024 65 3

        describe "deFEC" $ do
            it "is the inverse of enFEC" $ withMaxSuccess 200 prop_deFEC

        describe "decode" $ do
            it "is (nearly) the inverse of encode" $ withMaxSuccess 200 prop_decode
            it "works with total=255" $ property $ prop_decode (Params 1 255)
            it "works with required=255" $ property $ prop_decode (Params 255 255)

        describe "encode" $ do
            -- Since a single property won't result in parallel execution, add a
            -- few of these.
            replicateM_ 10 $
                it "returns copies of the primary block for all 1 of N encodings" $
                    property $
                        withMaxSuccess 10000 prop_primary_copies
