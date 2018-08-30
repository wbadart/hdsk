{- |
Module:       Hdsk.DecisionTree.Entropy
Description:  Measures of data entropy
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module implements various measures of entropy and information gain
in service of decision tree classification.
-}

module Hdsk.DecisionTree.Entropy
( entropy
, conditionalEntropy
) where

import Control.Applicative (Alternative)
import qualified Data.Map.Strict as M

import Hdsk.Util (count, countBy', length', lg, uniq')

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
