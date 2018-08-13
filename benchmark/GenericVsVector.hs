{- |
Module:   GenericVsVector
-}

import Control.Monad (replicateM)
import System.Random (randomRIO)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Criterion.Main (defaultMain, env, nf, bench, bgroup)

import Hdsk.Description

mkEnvN size = do
  xs <- sample size
  let xsVec = V.fromList xs
  return (xs, xsVec)

mkEnv = mkEnvN 1000000

main = defaultMain
  [
    -- env mkEnv $ \ ~(xs, vec) -> bgroup "mean"
    -- [ bench "vector"  $ nf mean vec
    -- , bench "foldable" $ nf mean3 vec ]

  -- , env mkEnv $ \ ~(xs, vec) -> bgroup "variance"
    -- [ bench "vector"  $ nf var vec
    -- , bench "foldable" $ nf var3 vec ]

  -- , env mkEnv $ \ ~(xs, vec) -> bgroup "standard deviation"
    -- [ bench "vector"  $ nf std vec
    -- , bench "foldable" $ nf std3 vec ]

    env mkEnv $ \ ~(xs, vec) -> bgroup "percentile"
    [ bench "vector" $ nf (percentile 50)  vec
    , bench "seq"    $ nf (percentile' 50) vec ]
  ]


-- Utilities

sample :: Int -> IO [Double]
sample n = replicateM n $ randomRIO (1, 6)
