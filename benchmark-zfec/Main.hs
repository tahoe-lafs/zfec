module Main where

import Codec.FEC (FECParams (paramK, paramN), decode, encode, fec)
import Control.Monad (replicateM)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nfAppIO)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import Data.List (unfoldr)
import System.Random (genByteString, mkStdGen)

main :: IO ()
main =
    defaultMain
        -- Run against some somewhat arbitrarily chosen configurations.  Notably,
        -- though, 94/100 matches the numbers recorded in the readme.
        [ env (fec 2 3) makeFECBenchmarks
        , env (fec 16 31) makeFECBenchmarks
        , env (fec 94 100) makeFECBenchmarks
        ]
  where

    makeFECBenchmarks = fecGroup [10 ^ 6]

    fecGroup sizes params =
        bgroup
            (show (paramK params) <> "/" <> show (paramN params))
            ( []
                ++ (decodePrimaryBenchmark params <$> sizes)
                ++ (decodeSecondaryBenchmark params <$> sizes)
                ++ (encodeBenchmark params <$> sizes)
            )

    encodeBenchmark params size =
        env (setupBlocks (paramK params) size) $
            benchmarkEncode params
    decodePrimaryBenchmark params size =
        env (setupBlocks (paramK params) size) $
            benchmarkPrimaryDecode params
    decodeSecondaryBenchmark params size =
        env (setupBlocks (paramK params) size) $
            benchmarkSecondaryDecode params

    setupBlocks :: Int -> Int -> IO [B.ByteString]
    setupBlocks k blockSize = pure $ makeBlocks k blockSize

    benchmarkEncode params blocks =
        bench ("encode blockSize=" <> showWithUnit (B.length $ head blocks)) $
            -- We choose normal form here because the typical thing to do with the
            -- result is serialize use all of the bytes (eg, to write them to a
            -- file or send them over the network) so they will certainly all be
            -- used.
            nfAppIO (uncurry encode) (params, blocks)

    benchmarkPrimaryDecode params blocks =
        bench ("decode [0..] blockSize=" <> showWithUnit (B.length $ head blocks)) $
            -- normal form here for the same reason as in benchmarkEncode.
            -- assign block numbers to use only primary blocks
            nfAppIO (uncurry decode) (params, (zip [0 ..] blocks))

    benchmarkSecondaryDecode params blocks =
        bench ("decode [" <> show n <> "..] blockSize=" <> showWithUnit (B.length $ head blocks)) $
            -- normal form here for the same reason as in benchmarkEncode.
            -- assign block numbers to use as many non-primary blocks as
            -- possible
            nfAppIO (uncurry decode) (params, (zip [n ..] blocks))
      where
        n = paramN params - paramK params

makeBlocks :: Int -> Int -> [B.ByteString]
makeBlocks k size = take k . go $ mkStdGen 42
  where
    go = uncurry ($) . bimap (:) go . genByteString size

data BytesUnit = B | KB | MB deriving (Eq, Ord, Enum, Show, Bounded)

bestUnit :: Int -> BytesUnit
bestUnit n
    | n < 1000 = minBound
    | maxBound == nextUnit = nextUnit
    | otherwise = succ nextUnit
  where
    nextUnit = bestUnit . (`div` 1000) $ n

showWithUnit :: Int -> String
showWithUnit n = show (scale n) <> show u
  where
    scale n = n `div` (10 ^ (3 * fromEnum u))

    u = bestUnit n
