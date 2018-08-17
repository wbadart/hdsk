{-|
Module:       Hdsk.Metrics
Description:  Methods for evaluating model quality
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

A standard set of metrics for evaluating both classification and
regression models. In Big-O notation, let /C/ represent the number of
classes and /n/ the number of predictions. In general, we assume /n >> C/,
so that /n/ terms dominate /C/ terms.
-}

module Hdsk.Metrics
( accuracy,    accuracyCM
, precision,   precisionCM
, recall,      recallCM
, specificity, specificityCM
, f1,          f1CM
, confusionMatrix
, tp, fp, tn, fn
, meanSqError
, meanAbsError
, explainedVariance
, r2score
, Zippable
) where

import Control.Monad (foldM)
import Control.Monad.ST (ST)
import Data.Matrix (Matrix)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.HashTable.ST.Cuckoo as H
import qualified Data.List as L
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Hdsk.Description (mean, var)

type HashTable s k v = H.HashTable s k v


-- ===== ACCURACY ===== --

-- | /O(n)/ Given the ground truth @yTrue@ and predictions @yPred@,
-- the expression @accuracy c yTrue yPred@ reports the accuracy of the
-- predictions, /(TP + TN) \/ (P + N)/. Here, @c@ is the list of class
-- labels, e.g. @["cat", "dog"]@. Accuracy represents the proportion of
-- classifications which were correct.
accuracy :: (Zippable z, Eq a, Fractional b) => [a] -> z a -> z a -> b
accuracy = ((accuracyCM .) .) . confusionMatrix

-- | /O(C^2)/ where /C/ is the number of classes. Calculate accuracy
-- from a confusion matrix, rather than a list of truths and
-- predictions.
accuracyCM :: Fractional a => Matrix Int -> a
-- accuracyCM = checkNotEmpty (\cm _ -> M.trace cm `divInt` sum cm)
accuracyCM cm | sum cm == 0 = 0
              | otherwise   = M.trace cm `divInt` sum cm


-- ===== PRECISION ===== --

-- | /O(n)/ Compute the precision (positive predictive value) of a list
-- of predictions, given the class list, target class label,
-- and ground truths. The below example show how to find precision for a
-- binary classifier.
--
-- >>> truth = ["cat", "cat", "not cat"]
-- >>> preds = ["cat", "not cat", "not cat"]
-- >>> precision ["cat", "not cat"] "cat" truth preds
-- 0.5
--
-- The expression /TP \/ (TP + FP)/ represents precision in terms of
-- counts of true and false predictions. It is the proportion of
-- positive predictions which are true.
--
-- Precision is undefined when no positive predictions are made.
precision :: (Zippable z, Eq a, Fractional b) =>
    [a] -> a -> z a -> z a -> b
precision = mkCMFunc precisionCM

-- | /O(C^2)/ Compute the precision directly from a confusion matrix.
-- The second argument is the class index within the confusion matrix.
-- For instance, given the class list
--
-- >>> ["dog", "cat"]
--
-- The index of class /dog/ is @1@ and the index of class /cat/ is @2@
-- (since the matrix is 1-indexed).
precisionCM :: Fractional a => Matrix Int -> Int -> a
precisionCM cm i | tp cm i + fp cm i == (0::Int) = undefined
                 | otherwise = tp cm i / (tp cm i + fp cm i)


-- ===== RECALL ===== --

-- | /O(n)/ Compute the recall (sensitivity, true positive/ hit rate) of a
-- list of predictions, given ground truth, class list, and target class
-- label. See @precision@ for discussion of arguments.
--
-- Recall, in terms of the confusion matrix, is /TP \/ (TP + FN)/, and
-- represents the proportion of positive predictions which were
-- classified as such.
--
-- Recall is undefined when there are no positive observations.
recall :: (Zippable z, Eq a, Fractional b) => [a] -> a -> z a -> z a -> b
recall = mkCMFunc recallCM

-- | /O(C^2)/ Compute recall from a confusion matrix for a specified
-- class. See @precisionCM@ for discussion on the class index argument.
recallCM :: Fractional a => Matrix Int -> Int -> a
recallCM cm i | tp cm i + fn cm i == (0::Int) = undefined
              | otherwise = tp cm i / (tp cm i + fn cm i)


-- ===== SPECIFICITY ===== --

-- | /O(n)/ Compute the specificity (true negative rate) of the
-- predictions given a list of class labels, a target class, and ground
-- truth. /TN \/ (FP + TN)/, the proportion of negative objects
-- correctly labeled.
--
-- Specificity is undefined when there are no negative truths.
specificity :: (Zippable z, Eq a, Fractional b) =>
    [a] -> a -> z a -> z a -> b
specificity = mkCMFunc specificityCM

-- | /O(C^2)/ Compute specificity from a confusion matrix for a
-- specified class. See @precisionCM@ for discussion on the class index
-- argument.
specificityCM :: Fractional a => Matrix Int -> Int -> a
specificityCM cm i | fp cm i + tn cm i == (0::Int) = undefined
                   | otherwise = tn cm i / (fp cm i + tn cm i)


-- ===== F!-SCORE ===== --

-- | /O(n)/ Compute the balanced f1-score of the model for a given
-- class.
f1 :: (Zippable z, Eq a, Fractional b) => [a] -> a -> z a -> z a -> b
f1 = mkCMFunc f1CM

-- | /O(C^2)/ Compute the f1-score from a confusion matrix. See
-- @precisionCM@ for a discussion of the class index argument.
f1CM :: Fractional a => Matrix Int -> Int -> a
f1CM cm i | tp cm i + fp cm i + fn cm i == (0::Int) = undefined
          | otherwise = 2 * tp cm i / (2 * tp cm i + fp cm i + fn cm i)



-- | /O(C^2 + n)/ where /C/ is the number of classes and /n/ the number
-- of predictions. In general, /n >> C/.
--
-- Generates the confusion matrix for the predictions of an
-- N-class predictor. The result will be a 1-indexed NxN matrix where
-- rows represent the predicted class and columns the actual class.
-- Classes are encoded as indices, where the index of a class within the
-- matrix corresponds to its index within the set of classes.
confusionMatrix :: (Zippable z, Eq a) => [a] -> z a -> z a -> Matrix Int
confusionMatrix classes yTrue yPred =
    foldr hit (M.zero n n) $ Hdsk.Metrics.zip yTrue yPred
  where n = length classes
        hit (yt, yp) cm = let old = cm M.! (predIdx, trueIdx)
                              -- matrices are 1-indexed, so add 1
                              predIdx = getIdx yp classes + 1
                              trueIdx = getIdx yt classes + 1
                          in M.setElem (old + 1) (predIdx, trueIdx) cm
        getIdx e xs = fromMaybe 0 $ L.elemIndex e xs

-- confusionHash :: Zippable z =>
--     z a -> z a -> ST s (HashTable s k (HashTable s k Int))
-- confusionHash = (foldM hit H.new .) . Hdsk.Metrics.zip
--   where hit ht (yt, yp) = ht

-- | Class of types which support the zip and zipWith operations.
class Foldable z => Zippable z where
  zip     :: z a -> z b -> z (a, b)
  zipWith :: (a -> b -> c) -> z a -> z b -> z c

instance Zippable Vector where
  zip     = V.zip
  zipWith = V.zipWith

instance Zippable [] where
  zip     = L.zip
  zipWith = L.zipWith


-- | /O(1)/ Count the true positives for a class in a given confusion
-- matrix.
tp :: Num a => Matrix Int -> Int -> a
tp cm i = fromIntegral $ cm M.! (i, i)

-- | /O(C)/ Count the false negatives for a class in a given confusion
-- matrix.
fn :: Num a => Matrix Int -> Int -> a
fn cm i = fromIntegral $ sum (M.getCol i cm) - tp cm i

-- | /O(C)/ Count the false positives for a class in a given confusion
-- matrix.
fp :: Num a => Matrix Int -> Int -> a
fp cm i = fromIntegral $ sum (M.getRow i cm) - tp cm i

-- | /O(C^2)/ Count the true negatives for a class in a given confusion
-- matrix.
tn :: Num a => Matrix Int -> Int -> a
tn cm i = fromIntegral $
    sum cm - sum (M.getRow i cm) - sum (M.getCol i cm) + tp cm i
    -- Add TP cell back in since it was subtracted twice


-- ===== REGRESSION METRICS ===== --

-- | /O(n)/ Find the mean squared error of the regression. The squared
-- error of an estimation is the square of its difference with the
-- corresponding observation. The first argument is the list of
-- observations, and the second is the list of corresponding
-- estimations.
meanSqError :: Floating a => [a] -> [a] -> a
meanSqError = mkMeanErrorFunc (**2)


-- | /O(n)/ Find the mean absolute error of the regression. The
-- absolute error of an estimate is the absolute value of its difference
-- with the corresponding observation.
meanAbsError :: Fractional a => [a] -> [a] -> a
meanAbsError = mkMeanErrorFunc abs


mkMeanErrorFunc :: Fractional a => (a -> a) -> [a] -> [a] -> a
mkMeanErrorFunc f = ((mean . map f) .) . L.zipWith (-)


-- | /O(n)/ Find the explained variance of a regression. Explained
-- variance measures the proportion of the observations is accounted for
-- by the regression.
explainedVariance :: (Eq a, Floating a) => [a] -> [a] -> a
explainedVariance yObs yEst
    | var yObs == 0 = 1
    | otherwise     = 1 - var (residuals yObs yEst) / var yObs


-- | /O(n)/ Find the coefficient of determination (R^2 value) of a
-- regression. Aka goodness of fit.
r2score :: (Eq a, Floating a) => [a] -> [a] -> a
r2score [] _ = error "empty list"
r2score _ [] = error "empty list"
r2score yObs yEst
  | var yObs == 0 = 1
  | otherwise = 1 - sum (map (**2) $ residuals yObs yEst)
                  / sum (map (**2) $ residuals yObs (repeat avg))
  where avg = mean yObs


-- ===== Utilities ===== --

divInt :: (Integral n, Fractional m) => n -> n -> m
x `divInt` y = fromIntegral x / fromIntegral y

mkCMFunc :: (Zippable z, Eq a, Fractional b) =>
  (Matrix Int -> Int -> b) -> [a] -> a -> z a -> z a -> b
mkCMFunc f classes c yTrue yPred =
    f cm $ case L.elemIndex c classes of
             Just i  -> i + 1
             Nothing -> error "unknown class name"
  where cm = confusionMatrix classes yTrue yPred

residuals :: Num a => [a] -> [a] -> [a]
residuals = L.zipWith (-)
