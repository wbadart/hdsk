{- |
Module:   GenericVsVector
-}

import Control.Monad (replicateM)
import System.Random (randomRIO)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Criterion.Main (defaultMain, env, nf, bench, bgroup)

import Hdsk.Description (mean, var, std)
import Hdsk.Description.Generic (genericMean, genericVar, genericStd)

mkEnvN size = do
  xs <- sample size
  let xsVec = V.fromList xs
  return (xs, xsVec)

mkEnv = mkEnvN 1000000

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
