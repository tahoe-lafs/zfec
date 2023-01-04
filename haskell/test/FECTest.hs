module Main where

import Test.Hspec

import qualified Codec.FEC as FEC
import qualified Data.ByteString as B
import Data.List (sortBy)
import System.IO (IOMode (..), withFile)
import System.Random

import Test.QuickCheck

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

checkEnFEC :: Int -> IO ()
checkEnFEC len = do
    testdata <- withFile "/dev/urandom" ReadMode (\handle -> B.hGet handle len)
    let [a, b, c, d, e] = FEC.enFEC 3 5 testdata
    if FEC.deFEC 3 5 [b, e, d] == testdata
        then return ()
        else fail "deFEC failure"

main :: IO ()
main = hspec $ do
    describe "FEC" $ do
        it "can divide" $ mapM_ checkDivide [1, 2, 3, 4, 10]
        it "decode is the inverse of encode" $ (withMaxSuccess 5000 prop_FEC)
        it "deFEC is the inverse of enFEC" $ mapM_ checkEnFEC [1, 2, 3, 4, 5, 1024 * 1024]
