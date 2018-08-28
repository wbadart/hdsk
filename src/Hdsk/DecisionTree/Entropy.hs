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
) where

import qualified Data.Map.Strict as M

import Hdsk.Util (count)

-- | /O(n log n)/ Calculate the total information entropy of a dataset.
entropy :: (Functor f, Foldable f, Eq label, Ord label, Floating a)
        => (tup -> label) -> f tup -> a
entropy getLabel dat = M.foldr prob 0 (count (fmap getLabel dat))
  where prob ct acc = let p = fromIntegral ct / len in acc - p * lg p
        len = fromIntegral (length dat)


-- ===== Utilities ===== --

lg :: Floating a => a -> a
lg = logBase 2
