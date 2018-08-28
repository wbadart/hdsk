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

-- | /O(???)/ Calculate the total information entropy of a dataset.
entropy :: Floating a => [tup] -> a
entropy dat = undefined


-- ===== Utilities ===== --

lg :: Floating a => a -> a
lg = logBase 2
