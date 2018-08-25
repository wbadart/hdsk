{- |
Module:       Hdsk.Bayesian.BinsSpec
Description:  Unit tests of binning and histogram utilities
-}

module Hdsk.Bayesian.BinsSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Bayesian.Bins

spec :: Spec
spec = do

  let mkbin k xs = bin (genIntervals k xs) xs

  describe "bins" $ do
    it "computes the bin index of every element" $
      mkbin 2 [0, 1, 2, 3, 4, 5] `shouldBe` [0, 0, 0, 1, 1, 1]

    it "is inclusive on the right boundary" $
      mkbin 2 [0, 1, 2, 3, 4] `shouldBe` [0, 0, 0, 1, 1]
