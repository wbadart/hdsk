{-|
Module:       Hdsk.Cluster.KMeansSpec
Description:  Unit tests for the Hdsk.Cluster.KMeans exports
-}

module Hdsk.Cluster.KMeansSpec (spec) where

import Control.Exception (evaluate)
import Data.List (nub)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, property)

import Hdsk.Util (shouldBeUndefined)

import Hdsk.Cluster.KMeans

spec :: Spec
spec =

  describe "kmeans" $

    it "correctly groups close points" $
      kmeans [[0, 0], [10, 10]]
             [[1, 0], [0, 1], [9, 8]]
        `shouldBe` [0, 0, 1]

    it "is undefined for 0 clusters" $
      shouldBeUndefined $ evaluate (kmeans [] [[1, 1]])
