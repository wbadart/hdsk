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
, percentile
, percentile'
-- , percentile, q1, q3
-- , median, iqr
-- , noOutliers
) where

import Data.List (sort)
import qualified Data.List as L
import Data.Sequence (Seq( (:<|) ))
import qualified Data.Sequence as S
import Data.Vector (Vector)
import qualified Data.Vector as V

mean :: (Foldable f, Fractional n) => f n -> n
-- ^ /O(n)/ Computes the arithmetic mean of a vector of doubles.
mean xs = sum xs / fromIntegral (length xs)

var :: (Foldable f, Functor f, Floating n) => f n -> n
-- ^ /O(n)/ Computes the unbiased variance of a vector of doubles.
var xs = sum (fmap sqDiff xs) / fromIntegral (length xs - 1)
  where sqDiff x = (x - avg) ** 2; avg = mean xs

std :: (Foldable f, Functor f, Floating n) => f n -> n
-- ^ /O(n)/ Computes the standard deviation of a vector of doubles.
std = sqrt . var

percentile :: (Eq n, RealFrac n) => n -> Vector n -> n
-- ^ /O(n log n)/ Finds the @p@th percentile of the vector of
-- doubles, i.e. the element which is greater than @p@% of the other
-- elements. Uses fractional interpolation when @p@ does not generate a
-- whole index.
--
-- Unfortunately, the equation for fractional interpolation breaks down
-- both at the bounds of lists and for lists shorter than 2 elements. On
-- the flip side, these special cases can be handled in /O(n)/ or even
-- /O(1)/ time for the singleton case.
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

percentile' :: (Foldable p, Selectable p, RealFrac n) => n -> p n -> n
percentile' 0   xs = minimum xs
percentile' 100 xs = maximum xs
percentile' p xs | whole idx = select k xs
                 | otherwise = mean (select k xs, select (k + 1) xs)
  where idx = p * fromIntegral (length xs) / 100 + 0.5
        k   = floor idx

select :: (Foldable p, Selectable p, Ord a) => Int -> p a -> a
-- ^ /O(n)/ Simple implementation of quickselct on @Sequence@s.
-- Undefined on empty sequences.
select k all | len + 1 == k = x
             | len      > k = select k left
             | otherwise    = select (k - len - 1) right
  where (left, right) = partition (< x) xs
        len = length left
        x = Hdsk.Description.head all; xs = Hdsk.Description.tail all

median :: Vector Double -> Double
-- ^ /O(n log n)/ Finds the median element the vector. Undefined for empty
-- vectors.
median = percentile 50


-- ===== Utilities ===== --
vecLen :: Num n => Vector a -> n
vecLen = fromIntegral . V.length

vecSorted :: Ord a => Vector a -> Vector a
vecSorted = V.fromList . sort . V.toList

whole :: RealFrac a => a -> Bool
whole x = x == fromIntegral (floor x)


class Foldable p => Selectable p where
  partition :: (a -> Bool) -> p a -> (p a, p a)
  head      :: p a -> a
  tail      :: p a -> p a

instance Selectable Vector where
  partition = V.partition
  head      = V.head
  tail      = V.tail
instance Selectable Seq where
  partition = S.partition
  head (x:<|_)  = x
  tail (_:<|xs) = xs
instance Selectable [] where
  partition = L.partition
  head      = L.head
  tail      = L.tail
