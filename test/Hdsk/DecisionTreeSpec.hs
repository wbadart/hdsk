{- |
Module:       Hdsk.DecisionTreeSpec
Description:  Unit tests of decision tree classifiers
-}

module Hdsk.DecisionTreeSpec (spec) where

import Data.List (nub)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Util (shouldLieBetween)

import Hdsk.DecisionTree

spec :: Spec
spec = do

  describe "mkCatTests" $ do
    it "produces predicates over categorical feature values" $
      map ($1) (mkCatTests id [1, 1, 2, 3])
        `shouldBe` [True, False, False]

    it "works on non-trivial cases!" $ do
      let dat = [["sunny", "yes"], ["cloudy", "no"], ["sunny", "no"]]
      [test x | test <- mkCatTests head dat, x <- dat]
        `shouldBe` [False, True, False, True, False, True]


  let outlookTests = map (\v t -> head t == v) . nub . map head
      datIO = map words . lines <$> readFile "util/weather.txt"

  describe "infoGain" $
    it ("computes the information gained by a branching strategy " ++
        "in terms of entropy") $ do
      dat <- datIO
      shouldLieBetween 0.245 0.247 $ infoGain last dat (outlookTests dat)

  describe "splitInfo" $
    it "computes information gained in terms of representation" $ do
      dat <- datIO
      shouldLieBetween 1.576 1.578 $ splitInfo id dat (outlookTests dat)

  describe "gainRatio" $
    it "balances infoGain and splitInfo" $ do
      dat <- datIO
      shouldLieBetween 0.155 0.157 $ gainRatio last dat (outlookTests dat)
