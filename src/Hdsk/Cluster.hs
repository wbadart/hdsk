{- |
Module:       Hdsk.Cluster
Description:  Implementation of the k-means clustering algorithm
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module provides an implementation of the k-means clustering
algorithm. K-means is well suited to problems where the number of
clusters is known and fixed (it is a parameter to the algorithm).
-}

module Hdsk.Cluster
( kmeans
, kmedoids
, kclusterer
, cluster
, improve
, meanSqDist
, midpoints
, centroid
, medoid
, closestTo
, minkowski
, distManhattan
, distEuclidean
, distHamming
) where

import Control.Monad.Zip (MonadZip, mzipWith)
import Data.Function (on)
import Data.List (elemIndex, minimumBy, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Hdsk.Description (mean)
import Hdsk.Numerical (terminate)
import Hdsk.Util (countElemHT', length')

-- | /O(inkD)/ Run the kmeans clustering algorithm over the given
-- dataset. Returns a list of cluster labels which corresponds 1-1 with
-- the input list of data points.
kmeans :: Int -> [[Double]] -> [Int]
kmeans = kclusterer 0.01 distEuclidean centroid

-- | /O(ikD*n^2)/ Run the kmedoids clustering algorithm over the given
-- dataset.
kmedoids :: Int -> [[Double]] -> [Int]
kmedoids = kclusterer 0.01 distEuclidean (medoid distEuclidean)

-- | Convenience function for creating clustering function. For
-- instance, 'kmeans', as defined in this module, is a 'kclusterer' with
-- 'distEuclidean' distance metric, 'centroid' center measure, and
-- /eta = 0.01/.
kclusterer :: Eq tup
           => Double                 -- ^ Minimum improvement /eta/
           -> (tup -> tup -> Double) -- ^ Distance metric between points
           -> ([tup] -> tup)         -- ^ Measure of center of cluster
           -> Int                    -- ^ The parameter /k/
           -> [tup]                  -- ^ The list of data points
           -> [Int]                  -- ^ Final clustering
kclusterer eta dist center k dat = terminate eta err
                                 $ improve dist center dat initial
  where initial = take (length dat) $ cycle [0..k-1]
        err     = meanSqDist dist center dat

-- | /O(inkD)/ where /n/ is the number of data points, /k/ is the number
-- of clusters, /D/ is the dimensionality of the data, and /i/ is the
-- number of iterations. Improve the quality of the clustering.
-- Generates an infinite list of clusterings.
improve :: (Eq tup, Ord d) =>
    (tup -> tup -> d) -> ([tup] -> tup) -> [tup] -> [Int] -> [[Int]]
improve dist metric dat = iterate (mkClustering . mkCentroids)
  where mkClustering = flip (cluster dist) dat
        mkCentroids  = flip (midpoints metric) dat

-- | /O(nkD)/ where /n/ is the number of data points, /k/ is the number
-- of clusters, and /D/ is the dimensionality of the data. Run one
-- iteration of the k-means algorithm. The parameter /k/, number of
-- clusters, is implied by the length of the list of initial centroids.
-- Returns a list of cluster labels (encoded as integers) which
-- correspond 1-1 with the given list of data points.
cluster :: (Eq tup, Ord d) => (tup -> tup -> d) -> [tup] -> [tup] -> [Int]
cluster dist cs = map $ toIdx . flip (closestTo dist) cs
  where toIdx c = fromMaybe (-1) $ elemIndex c cs

-- | /O(nD)/ Calculate the mean squared distance from each point in a
-- cluster to the centroid.
meanSqDist :: (Ord d, Floating d) =>
    (tup -> tup -> d) -> ([tup] -> tup) -> [tup] -> [Int] -> d
meanSqDist dist c dat cIdxs = mean $ zipWith (((**2) .) . dist) dat cents
  where cents  = map (midpoints c cIdxs dat !!) cIdxs

-- | /O(nD)/ where /n/ is the number of data points and /D/ is the
-- dimensionality of the data. Calculate the midpoints of clusters
-- according to a metric ('centroid', for instance).
midpoints :: ([tup] -> tup) -> [Int] -> [tup]-> [tup]
midpoints center clusters =
    mkPts . V.accum (flip (:)) initial . zip clusters
  where initial = V.generate (maximum clusters + 1) $ const []
        mkPts = V.toList . V.map center

-- | /O(nD)/ where /n/ is the number of points and /D/ is the
-- dimensionality of each point. Compute the mean of a list of
-- D-dimensional points.
centroid :: Fractional a => [[a]] -> [a]
centroid = map mean . transpose

-- | /O(n^2)/ Compute the medoid of a list of points.
medoid :: (Ord d, Fractional d) => (tup -> tup -> d) -> [tup] -> tup
medoid dist xs = minimumBy (compare `on` avgDistToOthers) xs
  where avgDistToOthers x = mean $ map (dist x) xs

-- | /O(kD)/ where /k/ is the number of points to consider and /D/ is
-- the dimensionality of the data. Select from a list of points that
-- which is closest to a given point /x/ according to a given distance
-- metric. For instance, in one dimension:
--
-- >>> closestTo (\x y -> abs $ sum (zipWith (-) x y)) [0] [[1], [2]]
-- >>> [1.0]
closestTo :: Ord d => (tup -> tup -> d) -> tup -> [tup] -> tup
closestTo dist x = minimumBy (compare `on` dist x)

-- | /O(D)/ where /D/ is the dimensionality of the vectors. General
-- distance metric between two vectors/ data points in the same vector
-- space. The first parameter @p@ corresponds to the /L^p/ space to
-- compute the norm (e.g. @p = 2@ is Euclidean distance).
minkowski :: Floating a => a -> [a] -> [a] -> a
minkowski p x y = sum (zipWith absDistP x y) ** (1 / p)
  where absDistP xi yi = abs (xi - yi) ** p

-- | Common case of 'minkowski' is with /p = 1/ for Manhattan distance.
distManhattan :: Floating a => [a] -> [a] -> a
distManhattan = minkowski 1

-- | Common case of 'minkowski' is with /p = 2/ for Euclidean distance.
distEuclidean :: Floating a => [a] -> [a] -> a
distEuclidean = minkowski 2

-- | /O(n)/ Compute the Hamming distance between to vectors.
distHamming :: (MonadZip f, Foldable f, Eq a, Floating d)
            => f a -> f a -> d
distHamming u v = countElemHT' (mzipWith (/=) u v) True / length' u
