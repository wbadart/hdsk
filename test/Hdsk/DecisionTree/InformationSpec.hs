{- |
Module:       Hdsk.DecisionTree.InformationSpec
Description:  Unit tests for measures of entropy
-}

module Hdsk.DecisionTree.InformationSpec (spec) where

import Data.List (nub)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Util (shouldLieBetween)

import Hdsk.DecisionTree.Information

spec :: Spec
spec = do

  let outlookTests = map (\v t -> head t == v) . nub . map head
      datIO = map words . lines <$> readFile "util/weather.txt"

  describe "infoGain" $
    it ("computes the information gained by a branching strategy " ++
        "in terms of entropy") $ do
      dat <- datIO
      shouldLieBetween 0.245 0.247 $ infoGain last dat (outlookTests dat)

  describe "splitInfo" $
    it "computes information gained in terms of representation" $ do
      dat <- datIO
      shouldLieBetween 1.576 1.578 $ splitInfo id dat (outlookTests dat)

  describe "gainRatio" $
    it "balances infoGain and splitInfo" $ do
      dat <- datIO
      shouldLieBetween 0.155 0.157 $ gainRatio last dat (outlookTests dat)

  describe "entropy" $ do
    it "finds expected entropy in base 2" $
      entropy id [1, 1, 2, 2] `shouldBe` 1

    it "is 0 for homogeneous lists" $
      entropy id [1, 1, 1, 1] `shouldBe` 0

    it "works for multi-class data" $
      shouldLieBetween 1.58 1.59 $ entropy id [1, 2, 3]

  describe "conditionalEntropy" $
    it "finds the conditional entropy of a certain branching" $ do
      dat <- datIO
      shouldLieBetween 0.690 0.694
        $ conditionalEntropy last dat (outlookTests dat)
