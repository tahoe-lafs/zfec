module Main where

import qualified Data.ByteString as B
import qualified Codec.FEC as FEC
import System.IO (withFile, IOMode(..))
import System.Random
import Data.List (sortBy)

import Test.QuickCheck

-- | Return true if the given @k@ and @n@ values are valid
isValidConfig :: Int -> Int -> Bool
isValidConfig k n
  | k >= n = False
  | k < 1 = False
  | n < 1 = False
  | otherwise = True

randomTake :: Int -> Int -> [a] -> [a]
randomTake seed n values = map snd $ take n sortedValues where
  sortedValues = sortBy (\a b -> compare (fst a) (fst b)) taggedValues
  taggedValues = zip rnds values
  rnds :: [Float]
  rnds = randoms gen
  gen = mkStdGen seed

testFEC k n len seed = FEC.decode fec someTaggedBlocks == origBlocks where
  origBlocks = map (\i -> B.replicate len $ fromIntegral i) [0..(k - 1)]
  fec = FEC.fec k n
  secondaryBlocks = FEC.encode fec origBlocks
  taggedBlocks = zip [0..] (origBlocks ++ secondaryBlocks)
  someTaggedBlocks = randomTake seed k taggedBlocks

prop_FEC :: Int -> Int -> Int -> Int -> Property
prop_FEC k n len seed =
  isValidConfig k n && n < 256 && len < 1024 ==> testFEC k n len seed

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

main = do
  mapM_ (check (defaultConfig { configMaxTest = 1000, configMaxFail = 10000 })) [prop_FEC]
  mapM_ checkDivide [1, 2, 3, 4, 10]
  mapM_ checkEnFEC [1, 2, 3, 4, 5, 1024 * 1024]
