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
( classify
, infoGain
, splitInfo
, gainRatio
, mkCatTests
, mkContTests
) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, mfilter)
import Data.List (find, maximumBy)
import Data.Function (on)
import Hdsk.DecisionTree.Information (entropy, conditionalEntropy)
import Hdsk.Util (length', countBy', lg, majorityLabel, uniq')

-- | Predict the label of an unobserved tuple, according to the provided
-- parameters.
classify :: (Foldable f, Alternative f, MonadPlus f, Eq label, Ord label)
         => (f tup -> [[tup -> Bool]]) -- ^ Function to generate possible
                                       -- branchings from the data
         -> (tup -> label) -- ^ Function which extracts the label from a
                           -- data object
         -> ((tup -> label) -> f tup -> [tup -> Bool] -> Double) -- ^ Split
                                 -- criteria, like 'infoGain' or 'gainRatio'
         -> f tup -- ^ The dataset as a list of data objects
         -> tup   -- ^ The unobserved instance
         -> label -- ^ The predicted label
classify mkBranchings getLabel criterion dat tup
  | length (uniq' $ fmap getLabel dat) <= 1 = majorityLabel getLabel dat
  | otherwise =
    let branching  = bestBranching getLabel criterion dat branchings
        branchings = mkBranchings dat
     in maybe undefined
        (\p -> classify' (mfilter p dat) tup)
        $ find ($tup) branching
  where classify' = classify mkBranchings getLabel criterion

-- | /O(n log n)/ Compute the branching predicates for the feature
-- values at the given index. The feature values at index @idx@ should
-- be categorical.
mkCatTests :: (Foldable f, Alternative f, Ord a)
           => (tup -> a) -> f tup -> f (tup -> Bool)
mkCatTests getVal = fmap (\v -> (==v) . getVal) . uniq' . fmap getVal

-- | /O(???)/ Compute the branching predicates for a continuous
-- predicate. Will always be a pair, where one branch claims the objects
-- with values less than or equal to the split point, and the other
-- those greater. The split optimizes the given criterion.
mkContTests :: (Foldable  f, Alternative f, Ord a)
            => (tup -> a) -> f tup -> f (tup -> Bool)
mkContTests _ _ = undefined

-- | /O(BX)/ where /B/ is the number of branchings to test and /X/ is
-- the cost of the branching criterion. Find the optimal branching
-- according to the given criterion.
bestBranching :: (Foldable f, Alternative f, Eq label, Ord label)
              => (tup -> label)
              -> ((tup -> label) -> f tup -> [tup -> Bool] -> Double)
              -> f tup
              -> [[tup -> Bool]]
              -> [tup -> Bool]
bestBranching getLabel criterion dat =
    maximumBy (compare `on` criterion getLabel dat)


-- ===== Information Measures ===== --

-- | /O(nC + n log n)/ Compute the information gain of the given
-- branching. Branches are encoded as a list of predicates over data
-- instances. An object belongs to whichever branch for which it passes
-- the predicate. Assumes that each data object will pass one and only
-- one of the branching predicates.
--
-- All the other split criteria defined in this module (e.g.
-- 'splitInfo', 'gainRatio') share the same interface.
infoGain :: (Foldable f, Alternative f, Eq label, Ord label)
         => (tup -> label) -- ^ Funciton to extract label of a tuple
         -> f tup          -- ^ Dataset
         -> [tup -> Bool]  -- ^ Branching, encoded as list of predicates
         -> Double         -- ^ Calculated information gain
infoGain getLabel dat branches =
  entropy getLabel dat - conditionalEntropy getLabel dat branches

-- | /O(nB)/ where /B/ is the number of branches. Compute the split info
-- of the branching.
splitInfo :: (Foldable f, Alternative f, Eq label, Ord label)
          => (tup -> label) -> f tup -> [tup -> Bool] -> Double
splitInfo _ dat branches =
  let bits c = (c / length' dat) * lg (c / length' dat)
  in  -sum (map (bits . (`countBy'` dat)) branches)

-- | /O(nB + nC + n log n)/ Compute the gain ratio of a branching.
gainRatio :: (Foldable f, Alternative f, Eq label, Ord label)
          => (tup -> label) -> f tup -> [tup -> Bool] -> Double
gainRatio getLabel dat branches =
  infoGain getLabel dat branches / splitInfo getLabel dat branches