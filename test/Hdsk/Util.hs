{-|
Module:       Hdsk.Util
Description:  Helper functions for project unit tests.
-}

module Hdsk.Util
( shouldLieBetween
, shouldBeUndefined
, shouldBeEmpty
, doubles
) where

import Test.Hspec (
  Expectation, shouldSatisfy, shouldThrow, errorCall)
import Test.QuickCheck (Gen, choose)

shouldLieBetween :: (Real r, Ord r, Show r) =>
  r -> r -> r -> Expectation
shouldLieBetween n m = flip shouldSatisfy ((&&) <$> (>=n) <*> (<=m))

shouldBeUndefined :: IO a -> Expectation
shouldBeUndefined = flip shouldThrow $ errorCall  "Prelude.undefined"

shouldBeEmpty :: IO a -> Expectation
shouldBeEmpty = flip shouldThrow $ errorCall "empty list"

doubles :: Gen Double
doubles = choose (-999999999, 999999999)
