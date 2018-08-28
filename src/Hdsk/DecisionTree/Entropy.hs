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

import qualified Data.Map.Strict as M

import Hdsk.Util (count, countBy)

-- | /O(n log n)/ Calculate the total information entropy of a dataset.
entropy :: (Functor f, Foldable f, Eq label, Ord label, Floating a)
        => (tup -> label) -> f tup -> a
entropy getLabel dat = M.foldr prob 0 (count (fmap getLabel dat))
  where prob ct acc = let p = fromIntegral ct / len in acc - p * lg p
        len = fromIntegral (length dat)

-- | /O(???)/ Compute the conditional entropy of splitting the dataset
-- over the given branches.
conditionalEntropy :: (Functor f, Foldable f,
                       Eq label, Ord label, Floating a)
                   => (tup -> label) -> f tup -> [tup -> Bool] -> a
conditionalEntropy getLabel dat branches = undefined


-- ===== Utilities ===== --

lg :: Floating a => a -> a
lg = logBase 2

countBy' :: (Foldable f, Num b) => (a -> Bool) -> f a -> b
countBy' = (fromIntegral .) . countBy
