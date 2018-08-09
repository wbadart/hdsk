{-|
Module:       Hsdk.Description
Description:  Collection of descriptive statistics
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains basic numerical descriptive statistics that are
applicable to a list of numbers.
-}

module Hdsk.Description
( mean, meanVec
, var, varVec
, std, stdVec
) where

import Data.List (genericLength)
import Data.Vector (Vector)
import qualified Data.Vector as V

mean :: (Real r, Fractional p) => [r] -> p
-- ^ Computes the arithmetic mean of a list of real numbers.
--
-- >>> mean [1, 3, 3, 4]
-- 2.75
-- >>> mean [10]
-- 10.0
mean [] = undefined
mean xs = realToFrac (sum xs) / genericLength xs

meanVec :: Vector Double -> Double
-- ^ Computes the arithmetic mean of a vector of doubles.
meanVec xs | V.null xs = undefined
           | otherwise = V.sum xs / fromIntegral (V.length xs)

var :: (Real p, Floating p) => [p] -> p
-- ^ Computes the variance of a list of numbers.
--
-- >>> var [1, 2]
-- 0.25
-- >>> var [10.12, 10.12]
-- 0.0
var xs = let avg = mean xs
             sqDiff x = (x - avg) ** 2
         in mean (map sqDiff xs)

varVec :: Vector Double -> Double
-- ^ Computes the variance of a vector of doubles.
varVec xs = let avg = meanVec xs
                sqDiff x = (x - avg) ** 2
            in meanVec (fmap sqDiff xs)

std :: (Real p, Floating p) => [p] -> p
-- ^ Computes the standard deviation of a list of numbers.
--
-- >>> std [1, 1, 1]
-- 0.0
-- >>> std [1, 2]
-- 0.5
std = sqrt . var

stdVec :: Vector Double -> Double
-- ^ Computes the standard deviation of a vector of doubles.
stdVec = sqrt . varVec
