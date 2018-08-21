{- |
Module:       Hdsk.NumericalSpec
Description:  Until tests of Hdsk.Numerical module
-}

module Hdsk.NumericalSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hdsk.Util (shouldBeUndefined)

import Hdsk.Numerical

spec :: Spec
spec =

  describe "terminate" $ do
    it "picks the first element which doesn't improve err by eta" $
      terminate 1 id [10, 8, 6, 5.5, 5] `shouldBe` 5.5

    it "is undefined for empty estimate sequences" $
      shouldBeUndefined $ evaluate (terminate 1 id [])

    it "is undefined for single estimates" $
      shouldBeUndefined $ evaluate (terminate 1 id [10])
