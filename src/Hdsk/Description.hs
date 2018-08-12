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
, noOutliers
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
-- elements. Uses fractional interpolation when @p@ does not generate a
-- whole index.
--
-- Unfortunately, the equation for fractional interpolation breaks down both
-- at the bounds of lists and for lists shorter than 2 elements. On the flip
-- side, these special cases can be handled in /O(n)/ or even /O(1)/ time
-- for the singleton case.
percentile 0   xs = V.minimum xs
percentile 100 xs = V.maximum xs
percentile p xs | V.null xs = undefined
                | vecLen xs == 1 = V.head xs
                | whole idx = xx V.! k
                -- Apply fractional interpolation
                | otherwise = (1 - f) * (xx V.! k) + f * (xx V.! (k + 1))
  where whole x = x == fromIntegral (floor x)
        idx = p * vecLen xs / 100 - 0.5
        k = floor idx; f = idx - fromIntegral k
        xx = vecSorted xs

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

noOutliers :: Vector Double -> Vector Double
-- ^ /O(n log n)/ Gives a copy of the input vector with all of the outliers
-- filtered out. Outliers are considered to be points beyond 1.5 IQR from
-- the median
noOutliers xs = V.filter (\x -> x >= med - 1.5*iq && x <= med + 1.5*iq) xs
  where med = median xs; iq = iqr xs


-- ===== Utilities ===== --
vecLen :: Num n => Vector a -> n
vecLen = fromIntegral . V.length

vecSorted :: Ord a => Vector a -> Vector a
vecSorted = V.fromList . sort . V.toList
