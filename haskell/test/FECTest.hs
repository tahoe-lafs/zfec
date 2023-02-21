{-# LANGUAGE DerivingStrategies #-}

module Main where

import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import Hedgehog (
    MonadGen,
    diff,
    evalIO,
    forAll,
    property,
 )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

-- | Valid ZFEC parameters.
data Params = Params
    { required :: Int -- aka k
    , total :: Int -- aka n
    }
    deriving (Show, Ord, Eq)

-- | A somewhat efficient generator for valid ZFEC parameters.
validParameters :: MonadGen m => m Params
validParameters = do
    k <- Gen.integral (Range.linear 1 255)
    n <- Gen.integral (Range.linear k 255)
    pure $ Params k n

validFECParameters :: MonadGen m => m FEC.FECParams
validFECParameters = do
    (Params k n) <- validParameters
    pure $ FEC.fec k n

tests :: TestTree
tests =
    testGroup
        "FEC"
        [ prop_secureCombine
        , prop_deFEC
        , prop_decode
        ]

prop_secureCombine :: TestTree
prop_secureCombine = testProperty "`secureCombine` is the inverse of `secureDivide n`" $
    property $ do
        -- secureDivide is insanely slow and memory hungry for large inputs.  Just
        -- pass some small inputs.  It's not clear it's worth fixing (or even
        -- keeping) these functions.  They don't seem to be used by anything.  Why
        -- are they here?
        sz <- forAll $ Gen.integral (Range.linear 1 1024)
        byte <- forAll $ Gen.integral Range.linearBounded
        divisor <- forAll $ Gen.integral (Range.linear 1 1024)
        let input = B.replicate (sz + 1) byte
        parts <- evalIO $ FEC.secureDivide divisor input
        diff input (==) (FEC.secureCombine parts)

prop_deFEC :: TestTree
prop_deFEC = testProperty "deFEC is the inverse of enFEC" $
    property $ do
        (Params k n) <- forAll validParameters
        testdata <- forAll $ Gen.bytes (Range.linear 1 1024)
        let allShares = FEC.enFEC k n testdata
        minimalShares <- forAll $ take k <$> Gen.shuffle allShares
        diff testdata (==) (FEC.deFEC k n minimalShares)

prop_decode :: TestTree
prop_decode = testProperty "decode is (nearly) the inverse of encode" $
    property $ do
        fec <- forAll validFECParameters
        len <- forAll $ Gen.integral (Range.linear 1 16384)

        let -- Construct some blocks.  Each will just be the byte corresponding
            -- to the block number repeated to satisfy the requested length.
            origBlocks = B.replicate len <$> [0 .. fromIntegral (FEC.paramK fec - 1)]

            -- Encode the data to produce the "secondary" blocks which (might)
            -- add redundancy to the original blocks.
            secondaryBlocks = FEC.encode fec origBlocks

            -- Tag each block with its block number because the decode API
            -- requires this information.
            taggedBlocks = zip [0 ..] (origBlocks ++ secondaryBlocks)

        -- Choose enough of the tagged blocks (some combination of original and
        -- secondary) to try to use for decoding.
        someTaggedBlocks <- forAll $ take (FEC.paramK fec) <$> Gen.shuffle taggedBlocks

        -- It should decode to the original.
        diff origBlocks (==) (FEC.decode fec someTaggedBlocks)
