{-|
Module:       Hdsk.Description
Description:  Collection of descriptive statistics
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains basic numerical descriptive statistics that are
applicable to a vector of doubles.
-}

module Hdsk.Description
( mean
, var
, std
, percentile, q1, q3
, median, iqr
) where

import Data.List (sort)
import Data.Vector (Vector)
import qualified Data.Vector as V

mean :: Vector Double -> Double
-- ^ /O(n)/ Computes the arithmetic mean of a vector of doubles.
mean xs | V.null xs = undefined
        | otherwise = V.sum xs / vecLen xs

var :: Vector Double -> Double
-- ^ /O(n)/ Computes the unbiased variance of a vector of doubles.
var xs | V.null xs = undefined
       | otherwise =
           let avg = mean xs; sqDiff x = (x - avg) ** 2
           in sum (fmap sqDiff xs) / (vecLen xs - 1)

std :: Vector Double -> Double
-- ^ /O(n)/ Computes the standard deviation of a vector of doubles.
std = sqrt . var

percentile :: Double -> Vector Double -> Double
-- ^ /O(n log n)/ Finds the @p@th percentile of the vector of
-- doubles, i.e. the element which is greater than @p@% of the other
-- elements. If @p@% names an existing element, it is chosen, else the
-- mean of the two closest is given (i.e. using midpoint interpolation).
percentile p xs | V.null xs = undefined
                | vecLen xs == 1 = V.head xs
                | whole idx = vecSorted xs V.! left
                | otherwise = mean $ V.slice left 2 (vecSorted xs)
  where idx = (p / 100) * vecLen xs
        left = floor idx - if ceiling idx == vecLen xs then 1 else 0
        whole x = x == fromIntegral (floor x) || p == 50

median :: Vector Double -> Double
-- ^ /O(n log n)/ Finds the median element the vector. Undefined for empty
-- vectors.
median = percentile 50

q1 :: Vector Double -> Double
-- ^ /O(n log n)/ Finds the first quartile of a vector of doubles.
q1 = percentile 25

q3 :: Vector Double -> Double
-- ^ /O(n log n)/ Finds the third quartile of a vector of doubles.
q3 = percentile 75

iqr :: Vector Double -> Double
-- ^ /O(n log n)/ Finds the interquartile range of a vector of doubles.
iqr = (-) <$> q3 <*> q1


-- ===== Utilities ===== --
vecLen :: Num n => Vector a -> n
vecLen = fromIntegral . V.length

vecSorted :: Ord a => Vector a -> Vector a
vecSorted = V.fromList . sort . V.toList
