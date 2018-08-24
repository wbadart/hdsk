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
( knn
, majorityLabel
) where

import Data.Heap (MaxPrioHeap)
import Data.Maybe (fromMaybe)
import qualified Data.Heap as H

import Hdsk.Cluster.KMeans (DistFunc)

-- | /O(nD log k)/ where /D/ is the cost of the distance function.
-- Select the @k@ nearest training instances to the argument tuple,
-- according to the given distance metric (greater value means more
-- dissimilar).
knn :: Fractional a => DistFunc a -> Int -> [[a]] -> [a] -> [[a]]
knn dist k dat x = map snd (H.toAscList $ foldr nearest H.empty dat)
  where nearest x' h
          | H.size h < k = H.insert (d, x') h
          | d < getMax h = H.insert (d, x') (getTail h)
          | otherwise    = h
          where d = dist x x'
        getTail :: MaxPrioHeap Double [a] -> MaxPrioHeap Double [a]
        getTail = fromMaybe H.empty . H.viewTail
        getMax = maybe (1/0) fst . H.viewHead

-- | /O(???)/ Report the most frequent label from a list of data
-- instances.
majorityLabel :: (t -> l) -> [t] -> l
majorityLabel _ _ = undefined
