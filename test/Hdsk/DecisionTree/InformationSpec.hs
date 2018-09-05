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

  describe "entropy" $ do
    it "finds expected entropy in base 2" $
      entropy id [1, 1, 2, 2] `shouldBe` 1

    it "is 0 for homogeneous lists" $
      entropy id [1, 1, 1, 1] `shouldBe` 0

    it "works for multi-class data" $
      shouldLieBetween 1.58 1.59 $ entropy id [1, 2, 3]


  describe "conditionalEntropy" $
    it "finds the conditional entropy of a certain branching" $ do
      dat <- map words . lines <$> readFile "util/weather.txt"
      let outlookTests = map (\v t -> head t == v) $ nub $ map head dat
      shouldLieBetween 0.690 0.694
        $ conditionalEntropy last dat outlookTests
