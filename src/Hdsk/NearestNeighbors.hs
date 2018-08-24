{-|
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

-- | /O(nD)/ where /D/ is the cost of the distance function. Select the
-- @k@ nearest training instances to the argument tuple, according to
-- the given distance metric (greater value means more dissimilar).
knn :: Fractional a =>
    DistFunc a -> Int -> [[a]] -> [a] -> MaxPrioHeap Double [a]
knn dist k dat x = foldr nearest (H.empty::MaxPrioHeap Double a) dat
  where nearest x' h =
          if dist x x' < getMax h
            then H.insert (dist x x', x')
               (if H.size h < k
                  then h
                  else fromMaybe undefined (H.viewTail h))
            else h
        getMax = fst . fromMaybe (1/0) . H.viewHead

-- | /O(???)/ Report the most frequent label from a list of data
-- instances.
majorityLabel :: (t -> l) -> [t] -> l
majorityLabel _ _ = undefined
