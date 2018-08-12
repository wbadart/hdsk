{-|
Module:       Hdsk.DescriptionSpec
Description:  Unit tests for the Hdsk.Description exports
-}

module Hdsk.DescriptionSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Vector as V

import Test.QuickCheck (property, forAll, shuffle, choose, vector)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Util (shouldLieBetween, shouldBeUndefined)
import Hdsk.Description

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
  let uut = [1, 2, 3, 4, 5]
  let lpercentile p xs = percentile p $ V.fromList xs
  describe "percentile" $ do

    it "finds the p-percentile of the list by sorting" $ do
      lpercentile 0   uut `shouldBe` 1
      lpercentile 30  uut `shouldBe` 2
      lpercentile 50  uut `shouldBe` 3
      lpercentile 70  uut `shouldBe` 4
      lpercentile 100 uut `shouldBe` 5

    it "works on all permutations of a list" $ property $
      forAll (shuffle uut) (\xs -> lpercentile 30 xs == 2)

    it "gives the maximum for 100th percentile" $ property $
      \xs -> null xs || lpercentile 100 xs == maximum xs

    it "gives the minimum for 0th percentile" $ property $
      \xs -> null xs || lpercentile 0 xs == minimum xs

    it "handles singleton vectors" $ property $
      forAll (choose (0, 100)) (\p -> lpercentile p [0] == 0)

    -- it "for any p and for any singleton, the percentile is the value"
    -- ^ not sure how to express this just yet. Look at Gen combinators

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (percentile 50 V.empty)


  let lmedian = median . V.fromList
  describe "median" $ do

    it "picks the middle element of an odd-lengthed list" $ do
      lmedian [1, 2, 3] `shouldBe` 2
      lmedian [3, 1, 2] `shouldBe` 2
      lmedian [31, 111.1, 12, 13, -10] `shouldBe` 13

    it "accurately interpolates between values in an even-length list" $ do
      lmedian [1, 2, 3, 4] `shouldBe` 2.5
      lmedian [3, 4, 2, 1] `shouldBe` 2.5
      lmedian [141, 16, 1.7, 12] `shouldBe` 14

    it "works on all permutations of the list" $ property $
      forAll (shuffle uut) (\xs -> lmedian xs == 3)

    it "gives the single element of a singleton vector" $ property $
      forAll (vector 1) $ \xs -> lmedian xs == head xs

    it "is undefined on empty lists" $
      shouldBeUndefined $ evaluate (median V.empty)


  let lq1 = q1 . V.fromList
  let lq3 = q3 . V.fromList
  describe "q1" $ do

    it "finds the first quartile of a vector of doubles" $
      lq1 uut `shouldBe` 1.75

    it "works on all permutations of a list" $ property $
      forAll (shuffle uut) (\xs -> lq1 xs == 1.75)

    it "is always no greater than q3" $ property $
      \xs -> null xs || lq1 xs <= lq3 xs

    it "gives the singleton's value when applied to one" $ property $
      forAll (vector 1) $ \xs -> lq1 xs == head xs

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (q1 V.empty)


  describe "q3" $ do

    it "finds the third quartile of a vector of doubles" $
      lq3 uut `shouldBe` 4.25

    it "works on all permutations of a list" $ property $
        forAll (shuffle uut) $ \xs -> lq3 xs == 4.25

    it "is always at least q1" $ property $
      \xs -> null xs || lq3 xs >= lq1 xs

    it "gives the singleton's value when applied to one" $ property $
      forAll (vector 1) $ \xs -> lq3 xs == head xs

    it "is undefined on empty vectors" $
      shouldBeUndefined $ evaluate (q3 V.empty)
