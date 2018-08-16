{- |
Module: Metrics
-}

import Criterion.Main (defaultMain, env, nf, bench, bgroup)
import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Hdsk.Util (ints)
import Hdsk.Metrics

-- Matrix function, list function, vector function
type LF = [Int]      -> Double
type MF = Matrix Int -> Double
type VF = Vector Int -> Double

main = defaultMain
  [ env mkPreds $ \ ~(c, yt, yp) -> bgroup "list"
    [ bench "accuracy"    $ nf (accuracy    c   yt :: LF) yp
    , bench "precision"   $ nf (precision   c 0 yt :: LF) yp
    , bench "recall"      $ nf (recall      c 0 yt :: LF) yp
    , bench "specificity" $ nf (specificity c 0 yt :: LF) yp
    , bench "f1"          $ nf (f1          c 0 yt :: LF) yp
    ]

  , env mkCM $ \cm -> bgroup "matrix"
    [ bench "accuracy"    $ nf (accuracyCM          :: MF) cm
    , bench "precision"   $ nf ((`precisionCM`   1) :: MF) cm
    , bench "recall"      $ nf ((`recallCM`      1) :: MF) cm
    , bench "specificity" $ nf ((`specificityCM` 1) :: MF) cm
    , bench "f1"          $ nf ((`f1CM`          1) :: MF) cm
    ]

  , env mkVec $ \ ~(c, yt, yp) -> bgroup "vector"
    [ bench "accuracy"    $ nf (accuracy    c   yt :: VF) yp
    , bench "precision"   $ nf (precision   c 0 yt :: VF) yp
    , bench "recall"      $ nf (recall      c 0 yt :: VF) yp
    , bench "specificity" $ nf (specificity c 0 yt :: VF) yp
    , bench "f1"          $ nf (f1          c 0 yt :: VF) yp
    ]
  ]


mkPreds :: IO ([Int], [Int], [Int])
mkPreds =
    (,,) <$> return [0, 1]
         <*> readVec "benchmark/yTest.csv"
         <*> readVec "benchmark/yPred.csv"


mkCM :: IO (Matrix Int)
mkCM = do
  (c, yt, yp) <- mkPreds
  return $ confusionMatrix c yt yp


mkVec :: IO ([Int], Vector Int, Vector Int)
mkVec = do
  (c, yt, yp) <- mkPreds
  return (c, V.fromList yt, V.fromList yp)


readVec :: FilePath -> IO [Int]
readVec path = map read . lines <$> readFile path
