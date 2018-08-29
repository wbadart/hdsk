{- |
Module:       Hdsk.UtilSpec
Description:  Unit tests for the utility module
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Hdsk.UtilSpec (spec) where

import Control.Monad.ST (runST)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import qualified Data.HashTable.Class as H
import qualified Data.Map.Strict as M

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
