{- |
Module:   Benchmark
-}

import Criterion.Main (defaultMain)
import qualified GenericVsVector as G

main = defaultMain G.benchmarks
