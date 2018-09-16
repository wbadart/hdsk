{- |
Module:       Hdsk.DecisionTree
Description:  Decision tree classification algorithms
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module facilitates the definition of decision tree classifiers with
tools for combining and parametrizing entropy measures, pruning tactics,
and more.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Hdsk.DecisionTree
( DecisionTree(..)
, Attribute(..)
, classify
, id3
) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, mfilter)
import Data.Function (on)
import Data.List (find, maximumBy)
import Hdsk.DecisionTree.Information (infoGain)
import Hdsk.Util (head', majorityLabel, nuniq, uniqList)

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

-- | An attribute is a value of a data object which is not its label; a
-- data objects attributes are the complete set of non-label values
-- which can be extracted from the data object. Each one can be encoded
-- as a function from a data object to the target value type, and
-- wrapping that function in the 'Attribute' type allows you to tag
-- which attributes should be treated as continuous, and which are
-- categorical (a distinction which must be made for certain
-- algorithms).
data Attribute tup v
  -- | In general, categorical variables are those which can only take
  -- on one of a certain, fixed set of discrete values. In the context
  -- of this data type, an attribute should be tagged as categorical if
  -- its branches should be encoded with equality. That is, when a tree
  -- branches on a 'Categorical' attribute, it will generate one branch
  -- for each of the feature values, and each will test whether an
  -- unobserved data object shares that feature value.
  = Categorical (tup -> v)
  -- | In general, ordinal data are values which can be meaningfully
  -- ordered relative to one another. This can be either discrete (e.g.
  -- military rank, grade in school) or continuous (temperature
  -- outside). In the context of this data type, attributes should be
  -- tagged as 'Ordinal' when the branches they generate should be
  -- encoded as a binary split over some pivot. That is, when a tree
  -- branches on an 'Ordinal' attribute, it generates two branches, one
  -- which accepts data objects with attribute values less than or equal
  -- to a certain pivot, and one for those with greater (the pivot value
  -- depends on the split criteria for the algorithm).
  | Ordinal (tup -> v)

-- | /O(X log D)/ where /X/ is the maximum number of feature values and
-- /D/ is the depth of the tree. Use the given decision tree to predict
-- a label for an unobserved data instance.
classify :: DecisionTree tup label -> tup -> label
classify (Decision _ label) _  = label
classify (Branches _ kids) tup = maybe undefined (`classify` tup)
                               $ find match kids
  where match (Branches p _) = p tup
        match (Decision p _) = p tup

-- | Generate a decision tree using the ID3 algorithm. Supports
-- continuous variables in the same manner as C4.5, by setting a
-- threshold which corresponds to two branches (for data less or equal
-- to the threshold, and for data greater than it).
id3 :: (Alternative f, MonadPlus f, Foldable f, Ord label, Ord v)
    => (tup -> label)     -- ^ Gets label from tuple
    -> [Attribute tup v]  -- ^ Attributes of dataset (see 'Attribute')
    -> f tup              -- ^ The dataset
    -> DecisionTree tup label
id3 = id3' undefined (const True)
  where
    id3' :: forall f tup label v.
           (Alternative f, MonadPlus f, Foldable f, Ord label, Ord v)
        => label
        -> (tup -> Bool)
        -> (tup -> label)
        -> [Attribute tup v]
        -> f tup
        -> DecisionTree tup label
    id3' fallback prop getLabel unused dat
      | homogenous getLabel dat = Decision prop (getLabel $ head' dat)
      | null unused = Decision prop (majorityLabel getLabel dat)
      | null dat    = Decision prop fallback
      | otherwise   = Branches prop
                    $ let (unused', branching) = bestBranching
                       in map (recur unused') branching

      where bestBranching :: ([Attribute tup v], [tup -> Bool])
            bestBranching = best snd branchings
            best f = maximumBy (compare `on` infoGain getLabel dat . f)

            branchings :: [([Attribute tup v], [tup -> Bool])]
            branchings = map (mkTests . (`splitAt` unused))
                             [0..length unused - 1]

            mkTests :: ([Attribute tup v], [Attribute tup v])
                    -> ([Attribute tup v], [tup -> Bool])
            mkTests (_, []) = undefined
            mkTests (u1, attr:u2) = (u1 ++ u2, getBranchings attr vals)
              where getBranchings (Categorical a) =
                      map (\v -> (==v) . a)
                    getBranchings (Ordinal a) =
                      best id . map (\v -> [(<=v) . a, (>v) . a])
                    vals = uniqList $ fmap attr' dat
                    attr' = case attr of Categorical f -> f
                                         Ordinal f -> f

            recur :: [Attribute tup v]
                  -> (tup -> Bool) -> DecisionTree tup label
            recur unused' p = id3' (majorityLabel getLabel dat)
                                p getLabel unused' (mfilter p dat)

-- | Construct a decision tree using the C4.5 algorithm.

-- | Signal whether a dataset is single-classed.
homogenous :: (Functor f, Foldable f, Ord label)
           => (tup -> label) -> f tup -> Bool
homogenous = (((==1) . nuniq) .) . fmap
