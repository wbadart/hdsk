{- |
Module: Metrics
-}

import Criterion.Main (defaultMain, env, nf, bench, bgroup)
import Data.Matrix (Matrix)

import Hdsk.Metrics

type CMF = Matrix Int -> Double
type LF  = [Int] -> Double

main = defaultMain
  [ env mkPreds $ \ ~(c, yt, yp) -> bgroup "list metrics"
    [ bench "accuracy"    $ nf (accuracy yt) yp
    , bench "precision"   $ nf (precision   c 0 yt :: LF) yp
    , bench "recall"      $ nf (recall      c 0 yt :: LF) yp
    , bench "specificity" $ nf (specificity c 0 yt :: LF) yp
    , bench "f1"          $ nf (f1          c 0 yt :: LF) yp
    ]

  , env mkCM $ \cm -> bgroup "matrix metrics"
    [ bench "accuracyCM"    $ nf (accuracyCM :: CMF) cm
    , bench "precisionCM"   $ nf ((`precisionCM`   1) :: CMF) cm
    , bench "recallCM"      $ nf ((`recallCM`      1) :: CMF) cm
    , bench "specificityCM" $ nf ((`specificityCM` 1) :: CMF) cm
    , bench "f1CM"          $ nf ((`f1CM`          1) :: CMF) cm
    ]]


mkPreds :: IO ([Int], [Int], [Int])
mkPreds =
    (,,) <$> return [0, 1]
         <*> readVec "benchmark/yTest.csv"
         <*> readVec "benchmark/yPred.csv"


mkCM :: IO (Matrix Int)
mkCM = do
  (c, yt, yp) <- mkPreds
  return $ confusionMatrix c yt yp


readVec :: FilePath -> IO [Int]
readVec path = map read . lines <$> readFile path
