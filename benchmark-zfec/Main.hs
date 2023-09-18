module Main where

import Codec.FEC (FECParams (paramK, paramN), decode, encode, fec, initialize)
import Control.Monad (replicateM)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import Data.List (unfoldr)
import System.Random (genByteString, mkStdGen)

main :: IO ()
main =
    defaultMain
        -- Run against some somewhat arbitrarily chosen configurations.  Notably,
        -- though, 94/100 matches the numbers recorded in the readme.
        [ env (setupFEC 2 3) makeFECBenchmarks
        , env (setupFEC 16 31) makeFECBenchmarks
        , env (setupFEC 94 100) makeFECBenchmarks
        ]
  where
    setupFEC :: Int -> Int -> IO FECParams
    setupFEC k n = do
        initialize
        pure (fec k n)

    makeFECBenchmarks = fecGroup [10 ^ 0, 10 ^ 3, 10 ^ 6]

    fecGroup sizes params =
        bgroup
            (show (paramK params) <> " of " <> show (paramN params))
            ( (\size -> env (setupBlocks (paramK params) size) $ benchmark params)
                <$> sizes
            )

    setupBlocks :: Int -> Int -> IO [B.ByteString]
    setupBlocks k blockSize = pure $ makeBlocks k blockSize

    benchmark params blocks =
        bench ("blockSize = " <> show (B.length $ head blocks)) $
            -- We choose normal form here because the typical thing to do with the
            -- result is serialize use all of the bytes (eg, to write them to a
            -- file or send them over the network) so they will certainly all be
            -- used.
            nf (uncurry encode) (params, blocks)

makeBlocks :: Int -> Int -> [B.ByteString]
makeBlocks k size = take k . go $ mkStdGen 42
  where
    go = uncurry ($) . bimap (:) go . genByteString size
