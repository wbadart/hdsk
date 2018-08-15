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
) where

import Data.Matrix (Matrix)
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Matrix as M


-- ===== ACCURACY ===== --

-- | /O(n)/ Given the ground truth @yTrue@ and predictions @yPred@,
-- the expression @accuracy c yTrue yPred@ reports the accuracy of the
-- predictions, /(TP + TN) \/ (P + N)/. Here, @c@ is the set of class
-- labels, e.g. @S.fromList ["cat", "dog"]@. The proportion of
-- classifications which were correct.
accuracy :: (Eq a, Fractional b) => [a] -> [a] -> [a] -> b
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
precision :: (Eq a, Fractional b) => [a] -> a -> [a] -> [a] -> b
precision = mkListFunc precisionCM

-- | /O(C^2)/ Compute the precision directly from a confusion matrix.
-- The second argument is the class index within the confusion matrix.
-- For instance, given the class list
--
-- >>> ["dog", "cat"]
--
-- The index of class /dog/ is @1@ and the index of class /cat/ is @2@
-- (since the matrix is 1-indexed).
precisionCM :: Fractional a => Matrix Int -> Int -> a
precisionCM cm i | tp cm i + fp cm i == 0 = undefined
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
recall :: (Eq a, Fractional b) => [a] -> a -> [a] -> [a] -> b
recall = mkListFunc recallCM

-- | /O(C^2)/ Compute recall from a confusion matrix for a specified
-- class. See @precisionCM@ for discussion on the class index argument.
recallCM :: Fractional a => Matrix Int -> Int -> a
recallCM cm i | tp cm i + fn cm i == 0 = undefined
              | otherwise = tp cm i / (tp cm i + fn cm i)


-- ===== SPECIFICITY ===== --

-- | /O(???)/ Compute the specificity (true negative rate) of the
-- predictions given a list of class labels, a target class, and ground
-- truth. /TN \/ (FP + TN)/
specificity :: (Eq a, Fractional b) => [a] -> a -> [a] -> [a] -> b
specificity = mkListFunc specificityCM

specificityCM :: Fractional a => Matrix Int -> Int -> a
specificityCM = checkNotEmpty (\cm i -> tn cm i / (fp cm i + tn cm i))


-- ===== F!-SCORE ===== --

-- | /O(???)/ Compute the f1-score of the model for a given class.
f1 :: (Eq a, Fractional b) => [a] -> a -> [a] -> [a] -> b
f1 = mkListFunc f1CM

f1CM :: Fractional a => Matrix Int -> Int -> a
f1CM = checkNotEmpty
    (\cm i -> 2 * tp cm i / (2 * tp cm i + fp cm i + fn cm i))



-- | /O(C^2 + n)/ where /C/ is the number of classes and /n/ the number
-- of predictions. In general, /n >> C/.
--
-- Generates the confusion matrix for the predictions of an
-- N-class predictor. The result will be a 1-indexed NxN matrix where
-- rows represent the predicted class and columns the actual class.
-- Classes are encoded as indices, where the index of a class within the
-- matrix corresponds to its index within the set of classes.
confusionMatrix :: Eq a => [a] -> [a] -> [a] -> Matrix Int
confusionMatrix classes yTrue yPred =
    foldr hit (M.zero n n) $ zip yTrue yPred
  where n = length classes
        hit (yt, yp) cm = let old = cm M.! (predIdx, trueIdx)
                              -- matrices are 1-indexed, so add 1
                              predIdx = getIdx yp classes + 1
                              trueIdx = getIdx yt classes + 1
                          in M.setElem (old + 1) (predIdx, trueIdx) cm
        getIdx e xs = fromMaybe 0 $ L.elemIndex e xs


-- ===== Utilities ===== -

divInt :: (Integral n, Fractional m) => n -> n -> m
x `divInt` y = fromIntegral x / fromIntegral y

mkListFunc :: (Eq a, Fractional b) =>
  (Matrix Int -> Int -> b) -> [a] -> a -> [a] -> [a] -> b
mkListFunc f classes c yTrue yPred =
    f cm $ case L.elemIndex c classes of
             Just i  -> i + 1
             Nothing -> error "unknown class name"

  where cm = confusionMatrix classes yTrue yPred

checkNotEmpty :: (Matrix Int -> Int -> a) -> Matrix Int -> Int -> a
checkNotEmpty f cm i | sum cm == 0 = error "empty list"
                     | otherwise   = f cm i

-- | /O(1)/
tp :: Num a => Matrix Int -> Int -> a
tp cm i = fromIntegral $ cm M.! (i, i)

-- | /O(C)/
fn :: Num a => Matrix Int -> Int -> a
fn cm i = fromIntegral $ sum (M.getCol i cm) - tp cm i

-- | /O(C)/
fp :: Num a => Matrix Int -> Int -> a
fp cm i = fromIntegral $ sum (M.getRow i cm) - tp cm i

-- | /O(C^2)/
tn :: Num a => Matrix Int -> Int -> a
tn cm i = fromIntegral $
    sum cm - sum (M.getRow i cm) - sum (M.getCol i cm) + tp cm i
-- Add TP cell back in since it was subtracted twice
