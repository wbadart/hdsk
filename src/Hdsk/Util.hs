{- |
Module:       Hdsk.Util
Description:  Handy utilities used throughout the library
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

This module contains several general utility functions used throughout
the library. Exporting this because they may be helpful to users too.
-}

{-# LANGUAGE LambdaCase #-}

module Hdsk.Util
( count
, countBy
, countHT
, countElemHT
) where

import Control.Monad.ST (ST, runST)
import Data.Hashable (Hashable)
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import qualified Data.HashTable.ST.Cuckoo as H
import qualified Data.Map.Strict as M

-- | /O(n log n)/ Given a container, construct a map from the unique
-- elements of the container to their frequency within the container.
count :: (Foldable f, Ord k) => f k -> Map k Int
count = foldr (flip (M.insertWith (+)) 1) M.empty

-- | /O(n)/ Given a container and a predicate, calculate the number of
-- elements which pass the predicate
countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy p = foldr (\x t -> if p x then t + 1 else t) 0

-- | /O(n)/ Given a container, construct a hash table from the unique
-- elements of the container to their frequency within the container. I
-- recommend this function over 'count' when 1) counting performance is
-- critical, AND 2) you're familiar with the ST monad.
countHT :: (Foldable f, Eq k, Hashable k)
        => f k -> ST s (HashTable s k Int)
countHT = foldr count' H.new
  where count' x ht = ht >>= (\h' -> H.mutate h' x
          (\case Just c  -> (Just (c + 1), h')
                 Nothing -> (Just 1, h')))

-- | /O(n)/ Convenience function for getting the count of a single
-- element from a container.
countElemHT :: (Foldable f, Eq k, Hashable k) => f k -> k -> Int
countElemHT xs x = fromMaybe 0 $ runST $ (`H.lookup` x) =<< countHT xs
