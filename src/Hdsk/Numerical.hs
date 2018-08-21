{- |
Module:       Hdsk.Numerical
Description:  Collection of numerical methods and the functions which
                support them
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains functions which support efficient numerical
methods, e.g. approximating derivatives.
-}

module Hdsk.Numerical
( terminate
) where

-- | Consume a stream of sequentially improving estimations until some
-- measure of error fails to improve by /eta/. Requires at least two
-- estimates to be produced by the stream. Assumes that error decreases
-- with each estimate.
terminate :: Double -> (a -> Double) -> [a] -> a
terminate _   _   []     = undefined
terminate eta err (x:xs) = terminate' (err x) xs
  where terminate' ePrev (x':xs') =
          let eCurr = err x'
          in if ePrev - eCurr < eta then x' else terminate' eCurr xs'
        terminate' _ [] = undefined
