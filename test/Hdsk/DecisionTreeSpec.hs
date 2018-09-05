{- |
Module:       Hdsk.DecisionTreeSpec
Description:  Unit tests of decision tree classifiers
-}

module Hdsk.DecisionTreeSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.DecisionTree.Information (infoGain)
import Hdsk.DecisionTree

spec :: Spec
spec = do

  let wdat =
        [ ["sunny", "arid", "yes"]
        , ["cloudy", "arid", "no"]
        , ["sunny", "humid", "no"]]

  describe "classify" $
    it "picks the best label for the unobserved tuple" $ do
      let b dat = map (\i -> mkCatTests (!!i) dat) [0..1]
      classify b last infoGain wdat ["cloudy", "humid", ""]
        `shouldBe` "no"

  describe "mkCatTests" $ do
    it "produces predicates over categorical feature values" $
      map ($1) (mkCatTests id [1, 1, 2, 3])
        `shouldBe` [True, False, False]

    it "works on non-trivial cases!" $
      [test x | test <- mkCatTests head wdat, x <- wdat]
        `shouldBe` [False, True, False, True, False, True]
