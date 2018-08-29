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
, mkCatTests
) where

import Hdsk.DecisionTree.Entropy (entropy, conditionalEntropy)
import Hdsk.Util (length', countBy', lg, majorityLabel)

import qualified Data.Set as S

-- | /O(n log n)/ Compute the branching predicates for the feature
-- values at the given index. The feature values at index @idx@ should
-- be categorical.
mkCatTests :: Ord a => (tup -> a) -> [tup] -> [tup -> Bool]
mkCatTests getVal = map (\v -> (==v) . getVal) . uniq . map getVal


-- ===== Information Measures ===== --

-- | /O(nC + n log n)/ Compute the information gain of the given
-- branching. Branches are encoded as a list of predicates over data
-- instances. An object belongs to whichever branch for which it passes
-- the predicate. Assumes that each data object will pass one and only
-- one of the branching predicates.
infoGain :: (Eq label, Ord label)
         => (tup -> label) -> [tup] -> [tup -> Bool] -> Double
infoGain getLabel dat branches =
  entropy getLabel dat - conditionalEntropy getLabel dat branches

-- | /O(nB)/ where /B/ is the number of branches. Compute the split info
-- of the branching.
splitInfo :: (Eq label, Ord label)
          => (tup -> label) -> [tup] -> [tup -> Bool] -> Double
splitInfo _ dat branches =
  let bits c = (c / length' dat) * lg (c / length' dat)
  in  -sum (map (bits . (`countBy'` dat)) branches)

-- | /O(nB + nC + n log n)/ Compute the gain ratio of a branching.
gainRatio :: (Eq label, Ord label)
          => (tup -> label) -> [tup] -> [tup -> Bool] -> Double
gainRatio getLabel dat branches =
  infoGain getLabel dat branches / splitInfo getLabel dat branches


-- ===== Utilities ===== --

-- | /O(n log n)/
uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList
