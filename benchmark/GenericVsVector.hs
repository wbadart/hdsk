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
    , bench "vector"  $ nf mean vec
    , bench "foldable" $ nf mean3 vec ]

  , env mkEnv $ \ ~(xs, vec) -> bgroup "variance"
    [ bench "generic" $ nf (genericVar::[Double]->Double) xs
    , bench "vector"  $ nf var vec
    , bench "foldable" $ nf var3 vec ]

  , env mkEnv $ \ ~(xs, vec) -> bgroup "standard deviation"
    [ bench "generic" $ nf (genericStd::[Double]->Double) xs
    , bench "vector"  $ nf std vec
    , bench "foldable" $ nf std3 vec ]]


-- Utilities

sample :: Int -> IO [Double]
sample n = replicateM n $ randomRIO (1, 6)

mean3 :: (Foldable f, Fractional n) => f n -> n
mean3 xs = sum xs / fromIntegral (length xs)

var3 :: (Functor f, Foldable f, Floating n) => f n -> n
-- ^ /O(n)/ Computes the unbiased variance of a vector of doubles.
var3 xs = let avg = mean3 xs; sqDiff x = (x - avg) ** 2
          in sum (fmap sqDiff xs) / fromIntegral (length xs - 1)

std3 :: (Functor f, Foldable f, Floating n) => f n -> n
-- ^ /O(n)/ Computes the standard deviation of a vector of doubles.
std3 = sqrt . var3
