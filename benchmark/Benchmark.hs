{- |
Module:   Benchmark
-}

import Control.Monad (replicateM)
import Data.List (sort)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random (randomRIO)

import Criterion.Main (defaultMain, env, nf, bench, bgroup)

import Hdsk.Description

sampleSize :: Int
sampleSize = 1000000

main = defaultMain
  [ env (sample sampleSize) $ \xs -> bgroup "test benchmark group"
    -- [ bench "reverse" $ nf reverse xs
    -- , bench "sort"    $ nf sort    xs]
    []

  , env (sample sampleSize) $ \xs -> bgroup "numerical description"
    [ bench "mean"         $ nf (mean::[Double]->Double) xs
    , bench "variance"     $ nf (var::[Double]->Double)  xs
    , bench "standard dev" $ nf (std::[Double]->Double)  xs]

  , env (sampleVec sampleSize) $ \xs -> bgroup "vector methods"
    [ bench "meanVec" $ nf meanVec xs
    , bench "varVec"  $ nf varVec xs
    , bench "stdVec"  $ nf stdVec xs]]


-- Utilities

sample :: Int -> IO [Double]
sample n = replicateM n $ randomRIO (1, 6)

sampleVec :: Int -> IO (Vector Double)
sampleVec n = V.replicateM n $ randomRIO (1, 6)
