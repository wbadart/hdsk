{- |
Module: NB
-}

import Criterion.Main (defaultMain, env, nf, bench, bgroup)

import Hdsk.Bayesian.NaiveBayes

main = defaultMain
  [ env mkDataset $ \dataset -> bgroup "naivebayes"
    [ bench "mkTables" $ nf mkTables dataset
    ]
  ]

mkDataset :: IO [[String]]
mkDataset =
  take 10000 . map read . lines <$> readFile "benchmark/playtennis.txt"
