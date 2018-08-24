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

import Hdsk.Cluster.KMeans (DistFunc)

-- | /O(nD)/ where /D/ is the cost of the distance function. Select the
-- @k@ nearest training instances to the argument tuple, according to
-- the given distance metric (greater value means more dissimilar).
knn :: Fractional a => DistFunc a -> Int -> [[a]] -> [[a]]
knn _ _ _ = undefined

-- | /O(???)/ Report the most frequent label from a list of data
-- instances.
majorityLabel :: (t -> l) -> [t] -> l
majorityLabel _ _ = undefined
