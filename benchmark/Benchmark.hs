{- |
Module:   Benchmark
-}

import Control.Monad (replicateM)
import System.Random (randomRIO)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Criterion.Main (defaultMain, env, nf, bench, bgroup)

import Hdsk.Description (
  mean, genericMean,
  var,  genericVar,
  std,  genericStd)

mkEnv = do
  xs <- sample 1000000
  let xsVec = V.fromList xs
  return (xs, xsVec)

main = defaultMain
  [ env mkEnv $ \ ~(xs, vec) -> bgroup "mean"
    [ bench "generic" $ nf (genericMean::[Double]->Double) xs
    , bench "vector"  $ nf mean vec ]

  , env mkEnv $ \ ~(xs, vec) -> bgroup "variance"
    [ bench "generic" $ nf (genericVar::[Double]->Double) xs
    , bench "vector"  $ nf var vec ]

  , env mkEnv $ \ ~(xs, vec) -> bgroup "standard deviation"
    [ bench "generic" $ nf (genericStd::[Double]->Double) xs
    , bench "vector"  $ nf std vec ]]


-- Utilities

sample :: Int -> IO [Double]
sample n = replicateM n $ randomRIO (1, 6)
