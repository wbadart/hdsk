{-|
Module:       Hdsk.MetricsSpec
Description:  Unit tests for the Hdsk.Metrics exports
-}

module Hdsk.MetricsSpec (spec) where

import Control.Exception (evaluate)
import Data.Matrix as M

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (
  Gen, arbitrary, property,
  choose, forAll, listOf1, orderedList, shuffle, vector)

import Hdsk.Util (
  doubles, shouldLieBetween, shouldBeUndefined, shouldBeErrorEmpty)

import Hdsk.Metrics

spec :: Spec
spec = do

  let classes = ["cat", "dog", "rabbit"]
      -- 8 cats, 6 dogs, 13 rabbit
      truth   = [ "cat", "cat", "cat", "cat", "cat", "cat", "cat", "cat"
                , "dog", "dog", "dog", "dog", "dog", "dog"
                , "rabbit" , "rabbit" , "rabbit" , "rabbit" , "rabbit"
                , "rabbit" , "rabbit" , "rabbit" , "rabbit" , "rabbit"
                , "rabbit" , "rabbit" , "rabbit" ]

      preds   = [ "cat", "cat", "cat", "cat", "cat", "dog", "dog", "dog"
                , "cat", "cat", "dog", "dog", "dog", "rabbit"
                , "dog", "dog", "rabbit", "rabbit", "rabbit", "rabbit"
                , "rabbit" , "rabbit" , "rabbit" , "rabbit" , "rabbit"
                , "rabbit" , "rabbit" ]

      cm      = confusionMatrix classes truth preds


  describe "confusion matrix" $

    it "tabulates the hits and misses" $
      cm `shouldBe` M.fromList 3 3
        [ 5, 2, 0
        , 3, 3, 2
        , 0, 1, 11 ]
