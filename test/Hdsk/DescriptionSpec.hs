{-|
Module:       Hdsk.DescriptionSpec
Description:  Unit tests for the Hdsk.Description exports
-}

module Hdsk.DescriptionSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Vector as V

import Test.QuickCheck (property, forAll, shuffle, choose)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Description (mean, var, std, percentile, q1, q3)
import Hdsk.Util (shouldLieBetween, shouldBeUndefined)

spec :: Spec
spec = do

  -- ===== MEAN ===== --
  let lmean = mean . V.fromList
  describe "mean" $ do

    it "averages the doubles in a vector" $ do
      lmean [1, 2, 3] `shouldBe` 2.0
      lmean [10, 20, 15, 31] `shouldBe` 19.0

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (mean V.empty)


  -- ===== VARIANCE ===== --
  let lvar = var . V.fromList
  describe "var" $ do

    it "correctly calculates the variance of a vector of doubles" $ do
      lvar [1, 2, 3] `shouldBe` 1
      lvar [10, 10, 10] `shouldBe` 0

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (var V.empty)


  -- ===== STANDARD DEVIATION ===== --
  let lstd = std . V.fromList
  describe "std" $ do

    it "correctly calculates the standard deviation of the vector" $ do
      lstd [1, 2, 3] `shouldBe` 1
      shouldLieBetween 1.58 1.59 (lstd [1, 2, 3, 4, 5])
      lstd [10, 10, 10] `shouldBe` 0

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (std V.empty)


  -- ===== PERCENTILE FAMILY ===== --
  let lpercentile p xs = percentile p $ V.fromList xs
  describe "percentile" $ do

    let uut = [1, 2, 3, 4, 5]
    it "finds the p-percentile of the list by sorting" $ do
      lpercentile 0   uut `shouldBe` 1.0
      lpercentile 25  uut `shouldBe` 2.0
      lpercentile 50  uut `shouldBe` 3.0
      lpercentile 75  uut `shouldBe` 4.0
      lpercentile 100 uut `shouldBe` 5.0

    it "works on all permutations of a list" $ property $
      forAll (shuffle uut) (\xs -> lpercentile 10 xs == 1.5)

    it "gives the maximum for 100th percentile" $ property $
      \xs -> null xs || lpercentile 100 xs == maximum xs

    it "gives the minimum for 0th percentile" $ property $
      \xs -> null xs || lpercentile 0 xs == minimum xs

    it "handles singleton vectors" $ property $
      forAll (choose (0.0, 1.0)) (\p -> lpercentile p [0] == 0)

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (percentile 50 V.empty)


  let lq1 = q1 . V.fromList
  describe "q1" $ do

    it "finds the first quartile of a vector of doubles" $
      lq1 [1, 2, 3, 4, 5] `shouldBe` 2

    it "works on all permutations of a list" $ property $
        forAll (shuffle [1, 2, 3, 4, 5]) (\xs -> lq1 xs == 2)

    it "never exceeds q3" $ property $
      (\xs -> V.null xs || q1 xs <= q3 xs) . V.fromList

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (q1 V.empty)


  describe "q3" $ do

    it "is never less than q1" $ property $
      (\xs -> V.null xs || q3 xs >= q1 xs) . V.fromList

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (q3 V.empty)
