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

import Hdsk.Description

spec :: Spec
spec = do

  -- ===== MEAN ===== --
  describe "mean" $ do

    it "averages the numbers in a list" $ do
      mean [1, 2, 3] `shouldBe` 2.0
      mean [10, 20, 15, 31] `shouldBe` 19.0

    it "handles lists of floats too" $
      mean [6.1, 99.1, 12.3, 44] `shouldBe` 40.375

    it "is undefined on empty lists" $
      evaluate (mean []) `shouldThrow` errorCall "Prelude.undefined"


  -- ===== VARIANCE ===== --
  describe "var" $ do

    it "correctly calculates the variance of a list of numbers" $
      var [1, 2, 3] `shouldBe` 2 / 3

    it "is undefined on empty lists" $
      evaluate (var []) `shouldThrow` errorCall "Prelude.undefined"


  -- ===== STANDARD DEVIATION ===== --
  describe "std" $ do

    it "correctly calculates the standard deviation of a list" $
      shouldLieBetween 0.80 0.82 (std [1, 2, 3])

    it "is undefined on empty lists" $
      evaluate (std []) `shouldThrow` errorCall "Prelude.undefined"


-- Utility functions

shouldLieBetween :: (Real r, Ord r, Show r) => r -> r -> r -> Expectation
shouldLieBetween n m = flip shouldSatisfy ((&&) <$> (>=n) <*> (<=m))
