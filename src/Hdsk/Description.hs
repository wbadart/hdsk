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
, median
, q1, q3, iqr
, select
, Selectable
) where

import Data.Sequence (Seq((:<|), Empty))
import Data.Vector (Vector)

import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Vector as V

-- | /O(n)/ Computes the arithmetic mean of a collection of numbers.
mean :: (Foldable f, Fractional n) => f n -> n
mean xs | null xs   = error "empty list"
        | otherwise = sum xs / fromIntegral (length xs)


-- | /O(n)/ Computes the unbiased variance of a collection of numbers.
var :: (Foldable f, Functor f, Floating n) => f n -> n
var xs | null xs = error "empty list"
       | length xs == 1 = 0
       | otherwise = sum (fmap sqDiff xs) / fromIntegral (length xs - 1)
  where sqDiff x = (x - avg) ** 2; avg = mean xs


-- | /O(n)/ Computes the standard deviation of a collection of numbers.
std :: (Foldable f, Functor f, Floating n) => f n -> n
std = sqrt . var


-- | /O(n)/ Selects the element which is greater than @p@% of the rest.
-- When the @p@-th percentile does not land directly on a whole index,
-- midpoint interpolation is used to average left and right side of the
-- split.
percentile :: (Selectable p, RealFrac n) => n -> p n -> n
percentile 0   xs = minimum xs
percentile 100 xs = maximum xs
percentile p xs | null xs = error "empty list"
                | length xs == 1 = Hdsk.Description.head xs
                | whole idx = select (k + 1) xs
                | otherwise = (select (k + 1) xs + select (k + 2) xs) / 2
  where idx = p * fromIntegral (length xs) / 100 - 0.5
        k   = floor idx


-- | /O(n)/ Finds the median element the collection.
median :: (Selectable p, RealFrac n) => p n -> n
median = percentile 50


-- | /O(n)/ Finds the first quartile of a collection of numbers.
q1 :: (Selectable p, RealFrac n) => p n -> n
q1 = percentile 25


-- | /O(n)/ Finds the third quartile of a collection of numbers.
q3 :: (Selectable p, RealFrac n) => p n -> n
q3 = percentile 75


-- | /O(n)/ Inter-quartile range. The distance between the first and third
-- quartiles.
iqr :: (Selectable p, RealFrac n) => p n -> n
iqr = (-) <$> q3 <*> q1


-- ===== Utilities ===== --

whole :: RealFrac a => a -> Bool
whole x = x == fromIntegral (floor x :: Int)


-- | /O(n)/ Simple implementation of quickselct (aka Hoare's algorithm or
-- k-rank). Selects the @k@-smallest element from the collection.
select :: (Selectable p, Ord a) => Int -> p a -> a
select k xxs | null xxs     = error "empty list"
             | len + 1 == k = x
             | len     >= k = select k left
             | otherwise    = select (k - len - 1) right
  where (left, right) = partition (< x) xs
        len = length left
        x = Hdsk.Description.head xxs; xs = Hdsk.Description.tail xxs


-- | Defines a container which is suitable for the k-rank/ select
-- algorithm.
class Foldable p => Selectable p where
  partition :: (a -> Bool) -> p a -> (p a, p a)
  head      :: p a -> a
  tail      :: p a -> p a


instance Selectable Vector where
  partition = V.partition
  head      = V.head
  tail      = V.tail

instance Selectable Seq where
  partition     = S.partition
  head (x:<|_)  = x
  head Empty    = error "empty list"
  tail (_:<|xs) = xs
  tail Empty    = error "empty list"

instance Selectable [] where
  partition = L.partition
  head      = L.head
  tail      = L.tail
