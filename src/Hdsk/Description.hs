{-|
Module:       Hsdk.Description
Description:  Collection of descriptive statistics
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains basic numerical descriptive statistics that are
applicable to a list of numbers.
-}

module Hdsk.Description
( mean
, var
, std
) where

import Data.List (genericLength)

mean :: (Real r, Fractional p) => [r] -> p
-- ^ Computes the arithmetic mean of a list of real numbers.
--
-- >>> mean [1, 3, 3, 4]
-- 2.75
-- >>> mean [10]
-- 10.0
mean [] = undefined
mean xs = realToFrac (sum xs) / genericLength xs

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

std :: (Real p, Floating p) => [p] -> p
-- ^ Computes the standard deviation of a list of numbers.
-- Examples:
--
-- >>> std [1, 1, 1]
-- 0.0
-- >>> std [1, 2]
-- 0.5
std = sqrt . var
