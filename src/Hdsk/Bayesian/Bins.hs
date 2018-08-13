{-|
Module:       Hdsk.Bayesian.Bins
Description:  Utilities for binning continuous data
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause
-}

module Hdsk.Bayesian.Bins
( bin
) where

import Data.List (find)
import Data.Vector (Vector)
import Hdsk.Description (percentile)

import qualified Data.Vector as V

bin :: Int -> Vector Double -> Vector Int
-- ^ /O(n k^2 n log n) where k is # of bins./ Map each number in a vector
-- of continueous data to a bin number according to its percentile (makes
-- even-width bins).
bin k xs | V.null xs = V.empty
         | otherwise = V.map idx xs
  where idx x = case find (($ x) . snd) (zip [1..k] preds) of
                  Just (i, _) -> i; Nothing -> k
        preds = map
          (\j y -> y <= percentile (100 * j / fromIntegral k) xs)
          [1..fromIntegral k]
