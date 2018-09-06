{- |
Module:       Hdsk.UtilSpec
Description:  Unit tests for the utility module
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Hdsk.UtilSpec (spec) where

import Control.Monad.ST (runST)
import Data.Vector (Vector)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import qualified Data.HashTable.Class as H
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Hdsk.Util

spec :: Spec
spec = do

  let uut = [1, 1, 2, 3] :: [Int]

  describe "majorityLabel" $
    it "picks the most frequent label from the datset" $
      majorityLabel id uut `shouldBe` 1

  describe "count" $
    it "finds the frequency of each element" $
      count uut `shouldBe` M.fromList [(1, 2), (2, 1), (3, 1)]

  describe "countBy" $
    it "gives the one-off count of elements passing the test" $
      countBy (==1) [1, 1, 2, 2, 1, 2] `shouldBe` 3

  describe "countBy'" $
    it "is equivalent to countBy" $ property
      (\(xs::[Int]) -> let p = (==1)
                       in  fromIntegral (countBy p xs) == countBy' p xs)

  describe "countHT" $
    it "finds the frequency of each element" $ do
      let ct = countHT uut
          expected = [(1, 2), (3, 1), (2, 1)]
      runST (H.toList =<< ct) `shouldBe` expected

  describe "countElemHT" $
    it "finds the frequency of one element" $
      countElemHT uut 1 `shouldBe` 2

  describe "length'" $
    it "is equivalent to length" $ property
      (\(xs::[Int]) -> fromIntegral (length xs) == length' xs)

  describe "lg" $
    it "is equivalent to logBase 2" $ property
      (\x -> x < 0 || lg x == logBase (2::Double) x)

  describe "uniq" $ do
    it "filters duplicates out of a list" $
      uniq [1, 1, 2, 3, 2] `shouldBe` [1, 2, 3]
    it "works on empty lists" $
      uniq ([]::[Int]) `shouldBe` []

  describe "uniq'" $ do
    it "filters duplicates out of a container" $
      uniq' (V.fromList [1, 1, 2, 3, 2]) `shouldBe` V.fromList [1, 2, 3]
    it "works on empty containers" $
      uniq' (V.empty::Vector Int) `shouldBe` V.empty

  describe "nuniq" $ do
    it "counts the number of unique elements in a container" $ do
      nuniq [1::Int, 2, 3, 4] `shouldBe` 4
      nuniq [1::Int, 1, 2, 2] `shouldBe` 2
    it "is zero on empty containers" $
      nuniq ([]::[Int]) `shouldBe` 0

  describe "head'" $
    it "works the same as head on lists" $ property
      (\(xs::[Int]) -> null xs || head xs == head' xs)

  describe "last'" $
    it "works the same as last on lists" $ property
      (\(xs::[Int]) -> null xs || last xs == last' xs)
