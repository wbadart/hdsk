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
  Gen, property, elements, forAll, listOf, listOf1, shuffle)

import Test.Util (
  doubles, ints,
  shouldLieBetween, shouldBeUndefined, shouldBeErrorEmpty)

import Hdsk.Metrics

spec :: Spec
spec = do

  describe "classification metrics" $ do

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


    -- ===== CLASSIFICATION METRICS ===== --

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
        forAll genCM (\ ~(cm', i) ->
          tp cm' i + fp cm' i == 0 || between (precisionCM cm' i) 0 1)


    describe "recall" $ do

      it "is the proportion of positive instances correctly labeled" $
        recallCM cm 1 `shouldBe` 5 / 8

      it "is undefined when no instances are truly positive" $
        shouldBeUndefined $ evaluate
                            (recall classes "cat" ["dog"] ["cat"])

      it "is always in the range [0, 1]" $ property $
        forAll genCM (\ ~(cm', i) ->
          tp cm' i + fn cm' i == 0 || between (recallCM cm' i) 0 1)


    describe "specificity" $ do

      it "is the proportion of negative instances correctly labeled" $
        specificityCM cm 1 `shouldBe` 17 / 19

      it "is undefined when no instances are truly negative" $
        shouldBeUndefined $ evaluate
                          (specificity classes "cat" ["cat"] ["cat"])

      it "is always in the range [0, 1]" $ property $
        forAll genCM (\ ~(cm', i) ->
          tn cm' i + fp cm' i == 0 || between (specificityCM cm' i) 0 1)


    describe "f1" $ do

      it "is the harmonic mean of precision and recall" $ do
        let p = precisionCM cm 1
            r = recallCM cm 1
        f1CM cm 1 `shouldBe` 2 / (1 / p + 1 / r)

      it ("is undefined when there are no "
            ++ "positive predictions and no positive observations") $
        shouldBeUndefined $ evaluate (f1 classes "cat" ["dog"] ["dog"])

      it "is always in the range [0, 1]" $ property $
        forAll genCM (\ ~(cm', i) ->
          tp cm' i + fp cm' i + fn cm' i == 0 || between (f1CM cm' i) 0 1)

      it "is always between precision and recall" $ property $
        forAll genCM (\ ~(cm', i) ->
          let p = precisionCM cm' i
              r = recallCM cm' i
              left = min p r
              right = max p r
          in tp cm' i + fp cm' i + fn cm' i == 0
          || between (f1CM cm' i) left right)


  describe "regression metrics" $ do

    let yObs  = [1,   2,   3,   4,   5]
        yEst  = [1.3, 2.4, 2.9, 3.8, 5.1]

    -- ===== REGRESSION METRICS ===== --

    describe "mean squared error" $ do

      it ("is the mean of the sqared differences "
            ++ "between prediction and observation") $
        meanSqError yObs yEst `shouldBe` (0.062::Double)

      it "is always positive" $ property $
        forAll genReg1 (\ ~(yt, yp) -> meanSqError yt yp >= 0)

      it "is undefined over 0 predictions" $
        shouldBeErrorEmpty $ evaluate (meanSqError [] [])


    describe "mean absolute error" $ do

      it "is the mean of the absolutes of the errors" $
        shouldLieBetween (0.21::Double) 0.22 (meanAbsError yObs yEst)

      it "is always positive" $ property $
        forAll genReg1 (\ ~(yt, yp) -> meanAbsError yt yp >= 0)

      it "is undefined for 0 predictions" $
        shouldBeErrorEmpty $ evaluate (meanAbsError [] [])


    describe "explained variance score" $ do

      it "correctly calculates the explained variance regression score" $
        explainedVariance yObs yEst `shouldBe` 0.974

      it "is no greater than 1" $ property $
        forAll genReg1 (\ ~(yt, yp) -> explainedVariance yt yp <= 1)

      it "is undefined over 0 predictions" $
        shouldBeErrorEmpty $ evaluate (explainedVariance [] [])


    describe "R^2 score" $ do

      it "correctly calculates the r^2 score of the regression" $
        r2score yObs yEst `shouldBe` 0.969

      -- Can be negative for arbitrarily poor models (i.e. worse than
      -- horizontal hyperplane)
      it "is always no more than 1" $ property $
        forAll genReg1 (\ ~(yt, yp) -> r2score yt yp <= 1)

      it "is undefined over 0 predictions" $
        shouldBeErrorEmpty $ evaluate (r2score [] [])


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

genReg1 :: Gen ([Double], [Double])
genReg1 = do
  yt <- listOf1 doubles
  yp <- shuffle yt
  return (yt, yp)

between :: Ord a => a -> a -> a -> Bool
between x i j = x >= i && x <= j
