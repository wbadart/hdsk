{-|
Module:       Hdsk.Cluster.KMeansSpec
Description:  Unit tests for the Hdsk.Cluster.KMeans exports
-}

module Hdsk.Cluster.KMeansSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

import Hdsk.Util (shouldLieBetween)

import Hdsk.Cluster.KMeans

spec :: Spec
spec = do

  let euclidean = minkowski (2 :: Double)
      center    = meanPoint
      dat       = [[1, 0], [0, 1], [9, 8]]
      cents     = [[0, 0], [10, 10]]

  describe "cluster" $ do

    it "correctly groups close points" $
      cluster euclidean cents dat `shouldBe` [0, 0, 1]

    it "should always give a cluster index" $ property
      (\dat' -> (-1) `notElem` cluster euclidean [[0, 0], [10, 10]] dat')


  describe "meanSqDist" $
    it ("averages the square distance from each " ++
        "point to its cluster's computed centroid") $
      shouldLieBetween 0.329 0.334 $ meanSqDist euclidean center dat [0, 0, 1]


  describe "centroids" $
    it "finds the mean point of each cluster" $
      centroids meanPoint [0, 0, 1] [[0, 0], [0, 1], [9, 8]]
      `shouldBe` [[0, 0.5], [9, 8]]


  describe "meanPoint" $
    it "finds the mean point in n dimensions of a list of points" $
      meanPoint [[1, 1], [2, 2], [4, 0]] `shouldBe` [7/3, 1]


  describe "closestTo" $
    it "for point x, selects the point from a list closest to x by `dist`" $
      closestTo euclidean [1, 1] [[0, 0], [10, 10]] `shouldBe` [0, 0]


  describe "minkowski" $ do

    it "finds the manhattan distance for p=1" $
      minkowski 1 [0, 0] [10, 10] `shouldBe` 20

    it "finds the euclidean distance for p=2" $
      minkowski 2 [0, 0] [10, 10] `shouldBe` sqrt 200
