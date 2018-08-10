{-|
Module:       Hdsk.Util
Description:  Helper functions for project unit tests.
-}

module Hdsk.Util (shouldLieBetween, shouldBeUndefined) where

import Test.Hspec (
  Expectation, shouldSatisfy, shouldThrow, errorCall)

shouldLieBetween :: (Real r, Ord r, Show r) =>
  r -> r -> r -> Expectation
shouldLieBetween n m = flip shouldSatisfy ((&&) <$> (>=n) <*> (<=m))

shouldBeUndefined :: IO a -> Expectation
shouldBeUndefined = flip shouldThrow $ errorCall  "Prelude.undefined"
