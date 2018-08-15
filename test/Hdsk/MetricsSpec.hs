{-|
Module:       Hdsk.MetricsSpec
Description:  Unit tests for the Hdsk.Metrics exports
-}

module Hdsk.MetricsSpec (spec) where

import Control.Exception (evaluate)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Matrix as M

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (
  Gen, property,
  choose, elements, forAll, listOf, listOf1, shuffle)

import Hdsk.Util (
  doubles, ints,
  shouldLieBetween, shouldBeUndefined, shouldBeErrorEmpty)

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


  describe "confusion matrix" $ do

    it "tabulates the hits and misses" $
      cm `shouldBe` M.fromList 3 3
        [ 5, 2, 0
        , 3, 3, 2
        , 0, 1, 11 ]

    it "is a matrix of zeros when no predictions given" $ property $
      forAll genPreds (\ ~(cs, _, _, _) ->
                      confusionMatrix cs [] []
                      == M.zero (length cs) (length cs))

    it "sums to the total number of predictions" $ property $
      forAll genPreds (\ ~(cs, _, yt, yp) ->
                      sum (confusionMatrix cs yt yp) == length yp)


  -- ===== METRICS ===== --

  describe "accuracy" $ do

    it "is the proportion of predictions which are correct" $
      accuracyCM cm `shouldBe` 19 / 27

    -- you miss 100% of the shots you don't take
    it "is always 0 when no predictions are made" $ property $
      forAll genPreds (\ ~(cs, _, _, _) -> accuracy cs [] [] == 0)

    it "is always in the range [0, 1]" $ property $
      forAll genPreds1 (\ ~(cs, _, yt, yp) ->
                       between (accuracy cs yt yp) 0 1)



  describe "precision" $ do

    it "is the proportion of positive predictions which are true" $
      precisionCM cm 1 `shouldBe` 5 / 7

    it "is undefined when no positive predicions are made" $
      shouldBeUndefined $ evaluate
                          (precision classes "cat" ["cat"] ["dog"])

    it "is always in the range [0, 1]" $ property $
      forAll genCM (\ ~(cm, i) ->
        tp cm i + fp cm i == 0 || between (precisionCM cm i) 0 1)


  describe "recall" $ do

    it "is the proportion of positive instances correctly labeled" $
      recallCM cm 1 `shouldBe` 5 / 8

    it "is undefined when no instances are truly positive" $
      shouldBeUndefined $ evaluate (recall classes "cat" ["dog"] ["cat"])

    it "is always in the range [0, 1]" $ property $
      forAll genCM (\ ~(cm, i) ->
        tp cm i + fn cm i == 0 || between (recallCM cm i) 0 1)


  describe "specificity" $ do

    it "is the proportion of negative instances correctly labeled" $
      specificityCM cm 1 `shouldBe` 17 / 19

    it "is undefined when no instances are truly negative" $
      shouldBeUndefined $ evaluate
                        (specificity classes "cat" ["cat"] ["cat"])

    it "is always in the range [0, 1]" $ property $
      forAll genCM (\ ~(cm, i) ->
        tn cm i + fp cm i == 0 || between (specificityCM cm i) 0 1)



genPreds :: Gen ([Int], Int, [Int], [Int])
genPreds = do
  cs <- listOf1 ints
  c  <- elements cs
  yt <- listOf (elements cs)
  yp <- shuffle yt
  return (cs, c, yt, yp)

genPreds1 :: Gen ([Int], Int, [Int], [Int])
genPreds1 = do
  cs <- listOf1 ints
  c  <- elements cs
  yt <- listOf1 (elements cs)
  yp <- shuffle yt
  return (cs, c, yt, yp)

genCM :: Gen (Matrix Int, Int)
genCM = do
  (cs, c, yt, yp) <- genPreds
  let cm = confusionMatrix cs yt yp
      i  = fromMaybe (-1) (elemIndex c cs) + 1
  return (cm, i)

between x i j = x >= i && x <= j
