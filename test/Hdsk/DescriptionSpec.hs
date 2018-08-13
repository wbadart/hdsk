{-|
Module:       Hdsk.DescriptionSpec
Description:  Unit tests for the Hdsk.Description exports
-}

module Hdsk.DescriptionSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Vector as V

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (
  Gen, arbitrary, property,
  choose, forAll, listOf1, orderedList, shuffle, vector)

import Hdsk.Util (
  doubles, shouldLieBetween, shouldBeUndefined, shouldBeErrorEmpty)
import Hdsk.Description

spec :: Spec
spec = do

  -- ===== MEAN ===== --
  describe "mean" $ do

    it "averages the doubles in a vector" $ do
      mean [1, 2, 3] `shouldBe` 2.0
      mean [10, 20, 15, 31] `shouldBe` 19.0

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (mean V.empty)


  -- ===== VARIANCE ===== --
  describe "var" $ do

    it "correctly calculates the variance of a vector of doubles" $ do
      var [1, 2, 3] `shouldBe` 1
      var [10, 10, 10] `shouldBe` 0

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (var V.empty)


  -- ===== STANDARD DEVIATION ===== --
  describe "std" $ do

    it "correctly calculates the standard deviation of the vector" $ do
      std [1, 2, 3] `shouldBe` 1
      shouldLieBetween 1.58 1.59 (std [1, 2, 3, 4, 5])
      std [10, 10, 10] `shouldBe` 0

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (std V.empty)


  -- ===== SELECT/ K-RANK ===== --
  describe "select" $ do

    it "picks the k-smallest element from a list" $ do
      select 2 [21, 24, 22] `shouldBe` 22
      select 9 [6, 1, 3, 5, 2, 9, 8, 4, 7, 10] `shouldBe` 9

    it "is `minumum` (non-empty) when k = 1" $ property $
      forAll (listOf1 doubles) (\xs -> select 1 xs == minimum xs)

    it "is `maximum` (non-empty) when k = len xs" $ property $
      forAll (listOf1 doubles)
             (\xs -> select (length xs) xs == maximum xs)

    it "is equivalent to 1-indexing on sorted lists" $ property $
      forAll orderedList
             (\xs -> null xs || select 1 xs == (head xs :: Int))

    it "is undefined on empty lists" $
      shouldBeErrorEmpty $ evaluate (select 1 ([]::[Int]))

    it "is undefined for k = 0" $
      shouldBeErrorEmpty $ evaluate (select 0 [0])


  -- ===== PERCENTILE FAMILY ===== --
  let uut = [1, 2, 3, 4, 5]
  describe "percentile" $ do

    it "finds the p-percentile of the list" $ do
      percentile 0   uut `shouldBe` 1
      percentile 25  uut `shouldBe` 1.5
      percentile 50  uut `shouldBe` 3
      percentile 75  uut `shouldBe` 4.5
      percentile 100 uut `shouldBe` 5

    it "works on all permutations of a list" $ property $
      forAll (shuffle uut) (\xs -> percentile 25 xs == 1.5)

    it "gives the maximum for 100th percentile" $ property $
      forAll (listOf1 doubles) (\xs -> percentile 100 xs == maximum xs)

    it "gives the minimum for 0th percentile" $ property $
      forAll (listOf1 doubles) (\xs -> percentile 0 xs == minimum xs)

    it "handles singleton vectors" $ property $
      forAll (choose (0, 100))
             (\p -> percentile p [0::Double] == (0::Double))

    -- it "for any p and for any singleton, the percentile is the value"
    -- ^ not sure how to express this just yet. Look at Gen combinators

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (percentile 50 V.empty)


  describe "median" $ do

    it "picks the middle element of an odd-lengthed list" $ do
      median [1, 2, 3] `shouldBe` 2
      median [3, 1, 2] `shouldBe` 2
      median [31, 111.1, 12, 13, -10] `shouldBe` 13

    it "accurately interpolates between values in an even-length list" $ do
      median [1, 2, 3, 4] `shouldBe` 2.5
      median [3, 4, 2, 1] `shouldBe` 2.5
      median [141, 16, 1.7, 12] `shouldBe` 14

    it "works on all permutations of the list" $ property $
      forAll (shuffle uut) (\xs -> median xs == 3)

    it "gives the single element of a singleton vector" $ property $
      forAll (vector 1) $ \xs -> median xs == (head xs :: Double)

    it "is undefined on empty lists" $
      shouldBeErrorEmpty $ evaluate (median V.empty)


  describe "q1" $ do

    it "finds the first quartile of a vector of doubles" $
      q1 uut `shouldBe` 1.5

    it "works on all permutations of a list" $ property $
      forAll (shuffle uut) (\xs -> q1 xs == 1.5)

    it "is always no greater than q3" $ property $
      forAll (listOf1 doubles) (\xs -> q1 xs <= q3 xs)

    it "gives the singleton's value when applied to one" $ property $
      forAll (vector 1) $ \xs -> q1 xs == (head xs :: Double)

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (q1 V.empty)


  describe "q3" $ do

    it "finds the third quartile of a vector of doubles" $
      q3 uut `shouldBe` 4.5

    it "works on all permutations of a list" $ property $
        forAll (shuffle uut) $ \xs -> q3 xs == 4.5

    it "is always at least q1" $ property $
      forAll (listOf1 doubles) (\xs -> q3 xs >= q1 xs)

    it "gives the singleton's value when applied to one" $ property $
      forAll (vector 1) $ \xs -> q3 xs == (head xs :: Double)

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (q3 V.empty)


  describe "iqr" $ do

    it "is equivalent to expanding the subtraction expression" $ property $
      forAll (listOf1 doubles) (\xs -> iqr xs == q3 xs - q1 xs)

    it "is undefined on empty vectors" $
      shouldBeErrorEmpty $ evaluate (iqr V.empty)
