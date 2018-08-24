{- |
Module:       Hdsk.NearestNeighborsSpec
Description:  Unit test for the KNN classification module
-}

module Hdsk.NearestNeighborsSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, elements, forAll, listOf, property, vectorOf)

import Hdsk.Cluster.KMeans (distEuclidean)
import Hdsk.Util (doubles)

import Hdsk.NearestNeighbors

spec :: Spec
spec = do

  describe "knnclassifier" $ do
    it "performs the full classification" $ do
      let dist x y = abs $ fst x - fst y
          vote = majorityLabel snd
          dat = [(0, "yes"), (1, "yes"), (0.5, "no"), (9, "no")]
      knnclassifier vote dist 3 dat (0, "unk") `shouldBe` "yes"

    it "works on the iris toy set" $ do
      let classify = knnclassifier (majorityLabel last) dist 5
          dist x y = distEuclidean (init x) (init y)
          getdat fname =
            map (read::String->[Double]) . lines <$> readFile fname
      train <- getdat "irisTrain.csv"
      test  <- getdat "irisTest.csv"
      map (classify train) test `shouldBe` map last test

  describe "knn" $ do
    it ("selects the single closest tuple to the target instance " ++
        "according to the distance metric with k=1") $
      knn distEuclidean 1 [[0, 0], [10, 10]] [1, 1] `shouldBe` [[0, 0]]

    it "selects the k closest tuples with larger k" $
      knn distEuclidean 3
        [ [0, 0] , [0, 1] , [1, 1]
        , [5, 6] , [6, 6] ]
        [0.5, 0.5] `shouldBe` [[1, 1], [0, 1], [0, 0]]

    it "never returns more than k points" $ property $
      forAll listOfPair
             (\ ~(dat, k) -> length (knn distEuclidean k dat [0, 0]) <= k)

  describe "majorityLabel" $
    it "reports the most frequent label" $
      majorityLabel id [0, 0, 0, 1, 1] `shouldBe` 0

listOfPair :: Gen ([[Double]], Int)
listOfPair = do
  k <- elements [1..99]
  dat <- listOf (vectorOf 2 doubles)
  return (dat, k)
