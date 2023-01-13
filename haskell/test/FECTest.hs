{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.Hspec

import Control.Monad.IO.Class (
    liftIO,
 )

import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List (sortBy)
import Data.Serializer
import Data.Word
import System.IO (IOMode (..), withFile)
import System.Random
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
        required <- choose (1, 255)
        total <- choose (required, 255)
        return $ Params required total

randomTake :: Int -> Int -> [a] -> [a]
randomTake seed n values = map snd $ take n sortedValues
  where
    sortedValues = sortBy (\a b -> compare (fst a) (fst b)) taggedValues
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
testFEC fec len seed =
    let -- Construct some blocks.  Each will just be the byte corresponding to the
        -- block number repeated to satisfy the requested length.
        origBlocks = B.replicate (fromIntegral len) . fromIntegral <$> [0 .. (FEC.paramK fec - 1)]
     in do
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

-- | @FEC.secureDivide@ is the inverse of @FEC.secureCombine@.
prop_divide :: Word16 -> Word8 -> Word8 -> Property
prop_divide size byte divisor = monadicIO $ do
    let input = B.replicate (fromIntegral size + 1) byte
    parts <- run $ FEC.secureDivide (fromIntegral divisor) input
    assert (FEC.secureCombine parts == input)

-- | @FEC.encode@ is the inverse of @FEC.decode@.
prop_decode :: Params -> Word16 -> Int -> Property
prop_decode (Params required total) len seed =
    monadicIO . run $ do
        fec <- FEC.fec required total
        testFEC fec len seed

-- | @FEC.enFEC@ is the inverse of @FEC.deFEC@.
prop_deFEC :: Params -> ArbByteString -> Property
prop_deFEC (Params required total) (ArbByteString testdata) = monadicIO $ do
    let testdataStrict = BL.toStrict testdata
    allShares <- run $ FEC.enFEC required total testdataStrict
    let minimalShares = take required allShares
    decoded <- run $ FEC.deFEC required total minimalShares
    assert $ decoded == testdataStrict

prop_primary_copies :: Params -> ArbByteString -> Property
prop_primary_copies (Params _ total) (ArbByteString primary) = monadicIO $ do
    fec <- run $ FEC.fec 1 total
    secondary <- run $ FEC.encode fec [BL.toStrict primary]
    assert $ all (BL.toStrict primary ==) secondary

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

        -- describe "deFEC" $ do
        --     it "is the inverse of enFEC" $ (withMaxSuccess 2000 prop_deFEC)

        -- describe "decode" $ do
        --     it "is (nearly) the inverse of encode" $ withMaxSuccess 2000 prop_decode
        --     it "works with total=255" $ property $ prop_decode (Params 1 255)
        --     it "works with required=255" $ property $ prop_decode (Params 255 255)

        describe "encode" $ do
            it "returns copies of the primary block for all 1 of N encodings" $ property $ withMaxSuccess 10000 prop_primary_copies
