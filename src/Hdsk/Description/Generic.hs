{-|
Module:       Hdsk.Description.Generic
Description:  Collection of descriptive statistics
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains basic numerical descriptive statistics that are
applicable to a list of numbers.
-}

module Hdsk.Description.Generic
( genericMean
, genericVar
, genericStd
) where

import Data.List (genericLength)

genericMean :: (Real r, Fractional p) => [r] -> p
-- ^ Computes the arithmetic mean of a list of real numbers.
--
-- >>> mean [1, 3, 3, 4]
-- 2.75
-- >>> mean [10]
-- 10.0
genericMean [] = undefined
genericMean xs = realToFrac (sum xs) / genericLength xs

genericVar :: (Real p, Floating p) => [p] -> p
-- ^ Computes the variance of a list of numbers.
--
-- >>> var [1, 2]
-- 0.25
-- >>> var [10.12, 10.12]
-- 0.0
genericVar xs = let avg = genericMean xs
                    sqDiff x = (x - avg) ** 2
                in genericMean (map sqDiff xs)

genericStd :: (Real p, Floating p) => [p] -> p
-- ^ Computes the standard deviation of a list of numbers.
--
-- >>> std [1, 1, 1]
-- 0.0
-- >>> std [1, 2]
-- 0.5
genericStd = sqrt . genericVar
