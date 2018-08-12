{-|
Module:       Hdsk.Description.GenericSpec
Description:  Unit tests for the Hdsk.Description exports
-}

module Hdsk.Description.GenericSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Description.Generic (genericMean, genericVar, genericStd)
import Hdsk.Util (shouldLieBetween, shouldBeUndefined)

spec :: Spec
spec = do

  describe "genericMean" $ do

    it "averages the numbers in a list" $ do
      genericMean [1, 2, 3] `shouldBe` 2.0
      genericMean [10, 20, 15, 31] `shouldBe` 19.0

    it "handles lists of floats too" $
      genericMean [6.1, 99.1, 12.3, 44] `shouldBe` 40.375

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (genericMean [])


  describe "genericVar" $ do
    it "correctly calculates the variance of a list of numbers" $
      genericVar [1, 2, 3] `shouldBe` 2 / 3

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (genericVar [])


  describe "genericStd" $ do

    it "correctly calculates the standard deviation of a list" $
      shouldLieBetween 0.80 0.82 (genericStd [1, 2, 3])

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (genericStd [])
