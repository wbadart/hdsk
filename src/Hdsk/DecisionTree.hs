{- |
Module:       Hdsk.DecisionTree
Description:  Decision tree classification algorithms
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module facilitates the definition of decision tree classifiers with
tools for combining and parametrizing entropy measures, pruning tactics,
and more.
-}

module Hdsk.DecisionTree
( infoGain
, splitInfo
, gainRatio
) where

import Hdsk.DecisionTree.Entropy (entropy, conditionalEntropy)
import Hdsk.Util (length', countBy', lg)

-- | /O(???)/
infoGain :: (Eq label, Ord label)
         => (tup -> label) -> [tup] -> [tup -> Bool] -> Double
infoGain getLabel dat branches =
  entropy getLabel dat - conditionalEntropy getLabel dat branches

-- | /O(???)/
splitInfo :: [tup] -> [tup -> Bool] -> Double
splitInfo dat branches =
  let bits c = (c / length' dat) * lg (c / length' dat)
  in  -sum (map (bits . (`countBy'` dat)) branches)

-- | /O(???)/
gainRatio :: (Eq label, Ord label)
          => (tup -> label) -> [tup] -> [tup -> Bool] -> Double
gainRatio getLabel dat branches =
  infoGain getLabel dat branches / splitInfo dat branches
