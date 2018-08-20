{- |
Module:       Hdsk.Cluster.KMeans
Description:  Implementation of the k-means clustering algorithm
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module provides an implementation of the k-means clustering
algorithm. K-means is well suited to problems where the number of
clusters is known and fixed (it is a parameter to the algorithm).
-}

module Hdsk.Cluster.KMeans
( DistFunc
, kmeans
, centroids
, meanPoint
, closestTo
, minkowski
) where

import Data.Function (on)
import Data.List (elemIndex, minimumBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Hdsk.Description (mean)

-- | A metric which rates, on a continuous scale, how far apart its two
-- arguments are.
type DistFunc a = [a] -> [a] -> Double

-- | /O(nkD)/ where /n/ is the number of data points, /k/ is the number
-- of clusters, and /D/ is the dimensionality of the data. Run one
-- iteration of the k-means algorithm. The parameter /k/, number of
-- clusters, is implied by the length of the list of initial centroids.
-- Returns a list of cluster labels (encoded as integers) which
-- correspond 1-1 with the given list of data points.
kmeans :: (Ord a, Floating a) => DistFunc a -> [[a]] -> [[a]] -> [Int]
kmeans dist centroids = map $ toIdx . flip (closestTo dist) centroids
  where toIdx c = fromMaybe (-1) $ elemIndex c centroids

-- | /O(nD)/ where /n/ is the number of data points and /D/ is the
-- dimensionality of the data. Calculate the centroids of clusters
-- according to a metric (@meanPoint@, for instance).
centroids :: Floating a => ([[a]] -> [a]) -> [Int] -> [[a]]-> [[a]]
centroids metric clusters = mkPts . V.accum (flip (:)) initial . zip clusters
  where initial = V.generate (maximum clusters + 1) $ const []
        mkPts = V.toList . V.map metric

-- | /O(nD)/ where /n/ is the number of points and /D/ is the
-- dimensionality of each point. Compute the mean of a list of
-- D-dimensional points.
meanPoint :: Fractional a => [[a]] -> [a]
meanPoint = map mean . transpose
  where transpose ([]:_) = []
        transpose x = map head x : transpose (map tail x)

-- | /O(kD)/ where /k/ is the number of points to consider and /D/ is
-- the dimensionality of the data. Select from a list of points that
-- which is closest to a given point /x/ according to a given distance
-- metric. For instance, in one dimension:
--
-- >>> closestTo (\x y -> abs $ sum (zipWith (-) x y)) [0] [[1], [2]]
-- >>> [1.0]
closestTo :: (Ord a, Floating a) => DistFunc a -> [a] -> [[a]] -> [a]
closestTo dist x = minimumBy (compare `on` dist x)

-- | /O(D)/ where /D/ is the dimensionality of the vectors. General
-- distance metric between two vectors/ data points in the same vector
-- space. The first parameter @p@ corresponds to the /L^p/ space to
-- compute the norm (e.g. @p = 2@ is Euclidean distance).
minkowski :: Floating a => a -> [a] -> [a] -> a
minkowski p x y = sum (zipWith absDistP x y) ** (1 / p)
  where absDistP xi yi = abs (xi - yi) ** p
