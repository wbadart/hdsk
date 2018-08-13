{- |
Module:      NaiveNBVsCPTNB

Compare the data-flow Naive Bayes impementation to the Map-/ conditional
probability table -based one.
-}

import Criterion.Main (defaultMain, bench, bgroup, nf)
import qualified Hdsk.Bayesian.NaiveBayes as NB1
import qualified Hdsk.Bayesian.OldNaiveBayes as NB2


main = defaultMain
  [ bgroup "new naive bayes"
    [ bench "classify" $ nf (nb1classify sampleData) uut ]
  , bgroup "old naive bayes"
    [ bench "classify" $ nf (NB2.classify sampleData) uut]]

uut         = ["sunny", "hot", "high", "false"]
nb1classify = NB1.classify . NB1.mkTables
sampleData  = [
  ["sunny", "hot", "high", "false", "no"],
  ["sunny", "hot", "high", "true", "no"],
  ["overcast", "hot", "high", "false", "yes"],
  ["rainy", "mild", "high", "false", "yes"],
  ["rainy", "cool", "normal", "false", "yes"],
  ["rainy", "cool", "normal", "true", "no"],
  ["overcast", "cool", "normal", "true", "yes"],
  ["sunny", "mild", "high", "false", "no"],
  ["sunny", "cool", "normal", "false", "yes"],
  ["rainy", "mild", "normal", "false", "yes"],
  ["sunny", "mild", "normal", "true", "yes"],
  ["overcast", "mild", "high", "true", "yes"],
  ["overcast", "hot", "normal", "false", "yes"],
  ["rainy", "mild", "high", "true", "no"]]
