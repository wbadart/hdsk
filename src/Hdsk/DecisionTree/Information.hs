{- |
Module:       Hdsk.DecisionTree.Information
Description:  Measures of data entropy
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module implements various measures of entropy and information gain
in service of decision tree classification.
-}

module Hdsk.DecisionTree.Information
( infoGain
, splitInfo
, gainRatio
, entropy
, conditionalEntropy
) where

import Control.Applicative (Alternative)
import qualified Data.Map.Strict as M

import Hdsk.Util (count, countBy', length', lg, uniq')


-- ===== Information Measures ===== ==

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


-- ===== Entropy Measures ===== --

-- | /O(n log n)/ Calculate the total information entropy of a dataset.
entropy :: (Functor f, Foldable f, Eq label, Ord label, Floating a)
        => (tup -> label) -> f tup -> a
entropy getLabel dat = M.foldr prob 0 (count (fmap getLabel dat))
  where prob ct acc = let p = fromIntegral ct / length' dat
                      in  acc - p * lg p

-- | /O(nC)/ Compute the conditional entropy of splitting the dataset
-- over the given branches.
conditionalEntropy :: (Foldable f, Alternative f, Eq label, Ord label)
                   => (tup -> label) -> f tup -> [tup -> Bool] -> Double
conditionalEntropy getLabel dat branches =
    let ct p    = countBy' p dat
        freqs p = (\l -> ct ((&&) <$> p <*> hasLabel l)) <$> labels dat
        fracs p = let c = ct p in (\f -> if f/=0 then (f/c)*lg(f/c) else 0)
                                  <$> freqs p
        total p = -sum (fracs p)
        ent p   = (ct p / length' dat) * total p
    in sum $ map ent branches
  where labels = uniq' . fmap getLabel
        hasLabel l x = getLabel x == l
