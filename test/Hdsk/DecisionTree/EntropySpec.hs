{- |
Module:       Hdsk.DecisionTree.EntropySpec
Description:  Unit tests for measures of entropy
-}

module Hdsk.DecisionTree.EntropySpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Util (shouldLieBetween)

import Hdsk.DecisionTree.Entropy

spec :: Spec
spec =

  describe "entropy" $ do
    it "finds expected entropy in base 2" $
      entropy id [1, 1, 2, 2] `shouldBe` 1

    it "is 0 for homogeneous lists" $
      entropy id [1, 1, 1, 1] `shouldBe` 0

    it "works for multi-class data" $
      shouldLieBetween 1.58 1.59 $ entropy id [1, 2, 3]
