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

  let dt = Branches (const True)
           [ Decision ((=="sunny")  . (!!0)) "play"
           , Branches ((=="cloudy") . (!!0))
             [ Decision ((=="warm") . (!!1)) "play"
             , Decision ((=="cold") . (!!1)) "not play" ]]

  describe "DecisionTree classify" $ do
    it "finds the applicable leaf node (Decision)" $
      classify dt ["sunny", "warm"] `shouldBe` "play"
    it "recurses correctly when need be" $
      classify dt ["cloudy", "cold"] `shouldBe` "not play"
    it "finds the first branching predicate to pass for the tuple" $
      classify dt ["cloudy", "warm"] `shouldBe` "play"
