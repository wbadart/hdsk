{-|
Module:       Hdsk.DescriptionSpec
Description:  Unit tests for the Hdsk.Description exports
-}

module Hdsk.DescriptionSpec (spec) where

import Control.Exception (evaluate)
import Data.Vector (empty, fromList)

import Test.Hspec (
  Spec, Expectation,
  describe, it,
  shouldBe, shouldSatisfy, shouldThrow,
  errorCall)

import Hdsk.Description (
  mean, genericMean,
  var, genericVar,
  std, genericStd)

spec :: Spec
spec = do


  -- ===== MEAN ===== --
  let lmean = mean . fromList
  describe "mean" $ do

    it "averages the doubles in a vector" $ do
      lmean [1, 2, 3] `shouldBe` 2.0
      lmean [10, 20, 15, 31] `shouldBe` 19.0

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (mean empty)

  describe "genericMean" $ do

    it "averages the numbers in a list" $ do
      genericMean [1, 2, 3] `shouldBe` 2.0
      genericMean [10, 20, 15, 31] `shouldBe` 19.0

    it "handles lists of floats too" $
      genericMean [6.1, 99.1, 12.3, 44] `shouldBe` 40.375

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (genericMean [])


  -- ===== VARIANCE ===== --
  let lvar = var . fromList
  describe "var" $ do

    it "correctly calculates the variance of a vector of doubles" $ do
      lvar [1, 2, 3] `shouldBe` 2 / 3
      lvar [10, 10, 10] `shouldBe` 0

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (var empty)

  describe "genericVar" $ do

    it "correctly calculates the variance of a list of numbers" $
      genericVar [1, 2, 3] `shouldBe` 2 / 3

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (genericVar [])


  -- ===== STANDARD DEVIATION ===== --
  let lstd = std . fromList
  describe "std" $ do

    it "correctly calculates the standard deviation of the vector" $ do
      shouldLieBetween 0.80 0.82 (lstd [1, 2, 3])
      lstd [10, 10, 10] `shouldBe` 0

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (std empty)

  describe "genericStd" $ do

    it "correctly calculates the standard deviation of a list" $
      shouldLieBetween 0.80 0.82 (genericStd [1, 2, 3])

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (genericStd [])

-- Utility functions

shouldLieBetween :: (Real r, Ord r, Show r) =>
  r -> r -> r -> Expectation
shouldLieBetween n m = flip shouldSatisfy ((&&) <$> (>=n) <*> (<=m))

shouldBeUndefined :: IO a -> Expectation
shouldBeUndefined = flip shouldThrow $ errorCall  "Prelude.undefined"
