{- |
Module:       Hdsk.Util
Description:  Handy utilities used throughout the library
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains several general utility functions used throughout
the library. Exporting this because they may be helpful to users too.
-}

module Hdsk.Util
( count
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | /O(n log n)/ Given a container, construct a map from the elements
-- of the container to their frequency within the container.
count :: (Foldable f, Ord k) => f k -> Map k Int
count = foldr (flip (M.insertWith (+)) 1) M.empty
