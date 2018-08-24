{- |
Module:       Hdsk.NearestNeighbors
Description:  Classify unobserved data based on similar instances
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module implements the K Nearest Neighbors classifier, which finds
the k tuples "closest" to the unobserved instance and combines their
labels via some voting/ consensus metric to produce a prediction.
-}

module Hdsk.NearestNeighbors
( knnclassifier
, knn
, majorityLabel
) where

import Data.Function (on)
import Data.Heap (MaxPrioHeap)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import qualified Data.Heap as H

-- | Assemble a KNN classifier.
knnclassifier :: Ord d
              => ([tup] -> label)   -- ^ Vote function
              -> (tup -> tup -> d)  -- ^ Distance function
              -> Int                -- ^ Parameter /k/
              -> [tup]              -- ^ Input data
              -> tup                -- ^ Unobserved instance
              -> label              -- ^ Predicted label
knnclassifier vote dist k dat = vote . knn dist k dat

-- | /O(nD log k)/ where /D/ is the cost of the distance function.
-- Select the @k@ nearest training instances to the argument tuple,
-- according to the given distance metric (greater value means more
-- dissimilar).
knn :: Ord d => (tup -> tup -> d) -> Int -> [tup] -> tup -> [tup]
knn dist k dat x = map snd (H.toAscList $ foldr nearest H.empty dat)
  where nearest x' h | H.size h < k = H.insert (d, x') h
                     | d < getMax h = H.insert (d, x') (getTail h)
                     | otherwise    = h
                     where d = dist x x'
        getTail :: Ord d => MaxPrioHeap d tup -> MaxPrioHeap d tup
        getTail = fromMaybe H.empty . H.viewTail
        getMax = maybe undefined fst . H.viewHead

-- | /O(k^2)/ Report the most frequent label from a list of data
-- instances.
majorityLabel :: Eq label => (tup -> label) -> [tup] -> label
majorityLabel getLabel dat = maximumBy (compare `on` count) labels
  where count x = length $ filter (==x) labels
        labels = map getLabel dat
