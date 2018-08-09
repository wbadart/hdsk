{-|
Module:       Hdsk.DescriptionSpec
Description:  Unit tests for the Hdsk.Description exports
-}

module Hdsk.DescriptionSpec (spec) where

import Control.Exception (evaluate)

import Test.Hspec (
  Spec, Expectation,
  describe, it,
  shouldBe, shouldSatisfy, shouldThrow,
  errorCall)

import Hdsk.Description (
  genericMean,
  genericVar,
  genericStd)

spec :: Spec
spec = do

  -- ===== MEAN ===== --
  describe "genericMean" $ do

    it "averages the numbers in a list" $ do
      genericMean [1, 2, 3] `shouldBe` 2.0
      genericMean [10, 20, 15, 31] `shouldBe` 19.0

    it "handles lists of floats too" $
      genericMean [6.1, 99.1, 12.3, 44] `shouldBe` 40.375

    it "is undefined on empty lists" $
      evaluate (genericMean []) `shouldThrow` errorCall "Prelude.undefined"


  -- ===== VARIANCE ===== --
  describe "genericVar" $ do

    it "correctly calculates the variance of a list of numbers" $
      genericVar [1, 2, 3] `shouldBe` 2 / 3

    it "is undefined on empty lists" $
      evaluate (genericVar []) `shouldThrow` errorCall "Prelude.undefined"


  -- ===== STANDARD DEVIATION ===== --
  describe "genericStd" $ do

    it "correctly calculates the standard deviation of a list" $
      shouldLieBetween 0.80 0.82 (genericStd [1, 2, 3])

    it "is undefined on empty lists" $
      evaluate (genericStd []) `shouldThrow` errorCall "Prelude.undefined"


-- Utility functions

shouldLieBetween :: (Real r, Ord r, Show r) =>
  r -> r -> r -> Expectation
shouldLieBetween n m = flip shouldSatisfy ((&&) <$> (>=n) <*> (<=m))
