{- |
Module:       Hdsk.Bins
Description:  Utilities for binning continuous data
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause
-}

module Hdsk.Bins
( bin
, genIntervals
) where

import Data.List (find)
import Hdsk.Description (Selectable, percentile)

-- | /O(n)/ Map each number in a vector of continuous data to a bin
-- number according to its percentile (makes even-width bins). Bins are
-- 0-indexed.
bin :: (Functor f, Selectable f) => [Double -> Bool] -> f Double -> f Int
bin intervals = fmap idx
  where idx x = case find (($ x) . snd) (zip [0..] intervals) of
                  Just (i, _) -> i
                  Nothing     -> length intervals - 1

-- | /O(kn)/ Generate the bounds of fixed width bins, encoded as
-- predicates of sample quantiles.
genIntervals :: Selectable f => Int -> f Double -> [Double -> Bool]
genIntervals k dat = map (\j y -> y <= percentile (100 * j / k') dat) [1..k']
  where k' = fromIntegral k
