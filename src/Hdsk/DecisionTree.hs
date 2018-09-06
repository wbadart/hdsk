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
( DecisionTree(..)
, classify
) where

import Control.Applicative (Alternative)
import Data.List (find)
import Hdsk.DecisionTree.Information (infoGain)
import Hdsk.Util (head', majorityLabel, nuniq)

-- | Representation of a decision tree's structure. Non-leaf nodes are
-- encoded with a predicate over the tuple type (this function signals
-- whether a tuple belongs to the branch) and its child nodes. Leaf
-- nodes, or decisions, likewise claim tuples with their embedded
-- predicate, and store the appropriate label.
data DecisionTree tup label
  -- | The first field of a branching node is the predicate which
  -- encodes it (e.g. "this is the branch for tuples with attribute
  -- weather = 'sunny'), and the second is the node's children.
  = Branches (tup -> Bool) [DecisionTree tup label]
  -- | Again, the first field determines which data objects this node
  -- applies to, and the second records the decision of the node.
  | Decision (tup -> Bool) label

-- | /O(X log D)/ where /X/ is the maximum number of feature values and
-- /D/ is the depth of the tree. Use the given decision tree to predict
-- a label for an unobserved data instance.
classify :: DecisionTree tup label -> tup -> label
classify (Decision _ label) _  = label
classify (Branches _ kids) tup = maybe undefined (`classify` tup)
                                $ find match kids
  where match (Branches p _) = p tup
        match (Decision p _) = p tup

-- | /O(???)/ Generate a decision tree using the ID3 algorithm.
id3 :: (Alternative f, Foldable f, Ord label)
    => label
    -> (tup -> Bool)
    -> (tup -> label)
    -> [tup -> v]
    -> f tup
    -> DecisionTree tup label
id3 fallback prop getLabel unused dat
  | homogenous  = Decision prop (getLabel $ head' dat)
  | null unused = Decision prop (majorityLabel getLabel dat)
  | null dat    = Decision prop fallback
  | otherwise   = Branches prop bestBranching
  where homogenous = nuniq (getLabel <$> dat) == 1
        bestBranching :: [DecisionTree tup label]
        bestBranching = undefined
