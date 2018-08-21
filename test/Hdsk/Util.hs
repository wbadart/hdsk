{-|
Module:       Hdsk.Util
Description:  Helper functions for project unit tests.
-}

module Hdsk.Util
( shouldLieBetween
, shouldBeUndefined
, shouldBeErrorEmpty
, doubles
, ints
) where

import Test.Hspec (
  Expectation, shouldSatisfy, shouldThrow, errorCall)
import Test.QuickCheck (Gen, choose)

shouldLieBetween :: (Real r, Ord r, Show r) =>
  r -> r -> r -> Expectation
shouldLieBetween n m = (`shouldSatisfy` ((&&) <$> (>=n) <*> (<=m)))

shouldBeUndefined :: IO a -> Expectation
shouldBeUndefined = (`shouldThrow` errorCall  "Prelude.undefined")

shouldBeErrorEmpty :: IO a -> Expectation
shouldBeErrorEmpty = (`shouldThrow` errorCall "empty list")

doubles :: Gen Double
doubles = choose (-999999999, 999999999)

ints :: Gen Int
ints = choose (0, 999999)
