{-|
Module:       Hdsk.Bayesian.NaiveBayesSpec
Description:  Unit tests for the Hdsk.Bayesian.NaiveBayes exports
-}

module Hdsk.Bayesian.NaiveBayesSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Bayesian.NaiveBayes

spec :: Spec
spec =

  describe "NaiveBayes" $

    it "classifies the weather data" $
      classify
        (mkTables sampleData)
        ["sunny", "cool", "normal", "false"]
      `shouldBe` "yes"


sampleData :: [[String]]
sampleData = [
  ["sunny", "hot", "high", "false", "no"],
  ["sunny", "hot", "high", "true", "no"],
  ["overcast", "hot", "high", "false", "yes"],
  ["rainy", "mild", "high", "false", "yes"],
  ["rainy", "cool", "normal", "false", "yes"],
  ["rainy", "cool", "normal", "true", "no"],
  ["overcast", "cool", "normal", "true", "yes"],
  ["sunny", "mild", "high", "false", "no"],
  ["sunny", "cool", "normal", "false", "yes"],
  ["rainy", "mild", "normal", "false", "yes"],
  ["sunny", "mild", "normal", "true", "yes"],
  ["overcast", "mild", "high", "true", "yes"],
  ["overcast", "hot", "normal", "false", "yes"],
  ["rainy", "mild", "high", "true", "no"]]
