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
( kmeans
) where

kmeans :: Fractional a => [[a]] -> [a] -> [a]
kmeans centroids dat = undefined
