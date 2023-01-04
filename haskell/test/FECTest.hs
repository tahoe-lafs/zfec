{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.Hspec

import qualified Data.ByteString.Lazy as BL
import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import Data.List (sortBy)
import System.IO (IOMode (..), withFile)
import System.Random
import Data.Int
import Data.Serializer

import Test.QuickCheck
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
        required <- choose (1, 254)
        total <- choose (min 255 (required + 1), 255)
        return $ Params required total

randomTake :: Int -> Int -> [a] -> [a]
randomTake seed n values = map snd $ take n sortedValues
  where
    sortedValues = sortBy (\a b -> compare (fst a) (fst b)) taggedValues
    taggedValues = zip rnds values
    rnds :: [Float]
    rnds = randoms gen
    gen = mkStdGen seed

testFEC :: Int -> Int -> Int -> Int -> Bool
testFEC k n len seed = FEC.decode fec someTaggedBlocks == origBlocks
  where
    origBlocks = map (\i -> B.replicate len $ fromIntegral i) [0 .. (k - 1)]
    fec = FEC.fec k n
    secondaryBlocks = FEC.encode fec origBlocks
    taggedBlocks = zip [0 ..] (origBlocks ++ secondaryBlocks)
    someTaggedBlocks = randomTake seed k taggedBlocks

prop_FEC :: Params -> Int -> Int -> Property
prop_FEC (Params k n) len seed = len < 1024 ==> testFEC k n len seed

checkDivide :: Int -> IO ()
checkDivide n = do
    let input = B.replicate 1024 65
    parts <- FEC.secureDivide n input
    if FEC.secureCombine parts == input
        then return ()
        else fail "checkDivide failed"


prop_deFEC :: Params -> ArbByteString -> Property
prop_deFEC (Params required total) (ArbByteString testdata) =
  FEC.deFEC required total minimalShares === testdataStrict
  where
    allShares = FEC.enFEC required total testdataStrict
    minimalShares = take required allShares
    testdataStrict = BL.toStrict testdata

main :: IO ()
main = hspec $ do
    describe "FEC" $ do
        it "secureCombine is the inverse of secureDivide n" $ mapM_ checkDivide [1, 2, 3, 4, 10]
        it "decode is (nearly) the inverse of encode" $ (withMaxSuccess 5000 prop_FEC)
    describe "deFEC" $ do
        it "is the inverse of enFEC" $ (withMaxSuccess 2000 prop_deFEC)

