import Criterion.Main (defaultMain, env, nf, bench, bgroup)

import Hdsk.Util (uniq, uniq', nuniq)

main = defaultMain
  [ bgroup "all unique values"
    [ bench "uniq"  $ nf uniq  [(1::Int)..10000]
    , bench "uniq'" $ nf uniq' [(1::Int)..10000]
    , bench "nuniq" $ nf nuniq [(1::Int)..10000] ]
  , bgroup "one unique values"
    [ bench "uniq"  $ nf uniq  $ replicate 10000 (0::Int)
    , bench "uniq'" $ nf uniq' $ replicate 10000 (0::Int)
    , bench "nuniq" $ nf nuniq $ replicate 10000 (0::Int) ]]
