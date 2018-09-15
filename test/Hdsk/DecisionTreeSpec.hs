{- |
Module:       Hdsk.DecisionTreeSpec
Description:  Unit tests of decision tree classifiers
-}

module Hdsk.DecisionTreeSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

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

  describe "id3" $
    it "constructs and ID3 decision tree" $ do
      dat <- map words . lines <$> readFile "util/weather.txt"
      let dt2 = id3 ""
                  (const True)
                  last
                  (map (Categorical . flip (!!)) [0..3])
                  dat
      classify dt2 ["sunny", "hot", "high", "false"] `shouldBe` "no"
