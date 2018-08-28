{- |
Module:       Hdsk.UtilSpec
Description:  Unit tests for the utility module
-}

module Hdsk.UtilSpec (spec) where

import Control.Monad.ST (runST)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.HashTable.Class as H
import qualified Data.Map.Strict as M

import Hdsk.Util

spec :: Spec
spec = do

  let uut = [1, 1, 2, 3] :: [Int]

  describe "count" $
    it "finds the frequency of each element" $
      count uut `shouldBe` M.fromList [(1, 2), (2, 1), (3, 1)]

  describe "countBy" $
    it "gives the one-off count of elements passing the test" $
      countBy (==1) [1, 1, 2, 2, 1, 2] `shouldBe` 3

  describe "countHT" $
    it "finds the frequency of each element" $ do
      let ct = countHT uut
          expected = [(1, 2), (3, 1), (2, 1)]
      runST (H.toList =<< ct) `shouldBe` expected

  describe "countElemHT" $
    it "finds the frequency of one element" $
      countElemHT uut 1 `shouldBe` 2
