{- |
Module:       Hdsk.UtilSpec
Description:  Unit tests for the utility module
-}

module Hdsk.UtilSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Map.Strict as M

import Hdsk.Util

spec :: Spec
spec =

  describe "count" $
    it "finds the frequency of each element" $
      count [1, 1, 2, 3] `shouldBe` M.fromList [(1, 2), (2, 1), (3, 1)]
