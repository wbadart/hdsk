{- |
Module:       Hdsk.NearestNeighborsSpec
Description:  Unit test for the KNN classification module
-}

module Hdsk.NearestNeighborsSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Cluster.KMeans (distEuclidean)

import Hdsk.NearestNeighbors

spec :: Spec
spec = do

  describe "knn" $ do
    it ("selects the single closest tuple to the target instance " ++
        "according to the distance metric with k=1") $
      knn distEuclidean 1 [[0, 0], [10, 10]] [1, 1] `shouldBe` [[0, 0]]

    it "selects the k closest tuples with larger k" $
      knn distEuclidean 3
        [ [0, 0] , [0, 1] , [1, 1]
        , [5, 6] , [6, 6] ]
        [0.5, 0.5] `shouldBe` [[1, 1], [0, 1], [0, 0]]

  describe "majorityLabel" $
    it "reports the most frequent label" $
      majorityLabel id [0, 0, 0, 1, 1] `shouldBe` 0
