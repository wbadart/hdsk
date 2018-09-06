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
( majorityLabel
, count
, countBy
, countBy'
, countHT
, countElemHT
, length'
, lg
, uniq
, uniq'
, nuniq
, head'
, last'
) where

import Control.Applicative (Alternative, (<|>), empty, pure)
import Control.Monad.ST (ST, runST)
import Data.List (maximumBy)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import qualified Data.HashTable.ST.Cuckoo as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | /O(n log n)/ Compute the most frequent label in the data set. Ties
-- do not have well defined behavior; the tie is broken by
-- 'Data.List.maximumBy' which doesn't have clear documentation on ties.
majorityLabel :: (Foldable f, Functor f, Eq label, Ord label)
              => (tup -> label) -> f tup -> label
majorityLabel getLabel =
  fst . maximumBy (compare `on` snd) . M.toList . count . fmap getLabel

-- | /O(n log n)/ Given a container, construct a map from the unique
-- elements of the container to their frequency within the container.
count :: (Foldable f, Ord k) => f k -> Map k Int
count = foldr (flip (M.insertWith (+)) 1) M.empty

-- | /O(n)/ Given a container and a predicate, calculate the number of
-- elements which pass the predicate
countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy p = foldr (\x t -> if p x then t + 1 else t) 0

-- | /O(n)/ Version of 'countBy' with generic result.
countBy' :: (Foldable f, Num b) => (a -> Bool) -> f a -> b
countBy' = (fromIntegral .) . countBy

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

-- | /O(n)/ Shorthand for 'genericLength' which works on all foldables.
length' :: (Num a, Foldable f) => f b -> a
length' = fromIntegral . length

-- | /O(1)/ Shorthand for @logBase 2@
lg :: Floating a => a -> a
lg = logBase 2

-- | /O(n log n)/ Filter the duplicates out of a list.
uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

-- | /O(n log n + nX)/ where /X/ is the const of the associative binary
-- operator for the given @Alternative@ type. Filter the duplicates out
-- of a @Foldable@, @Alternative@ container.
uniq' :: (Foldable f, Alternative f, Ord a) => f a -> f a
uniq' xs = S.foldr ((<|>) . pure) empty $ foldr S.insert S.empty xs

-- | /O(n)/ Return the number of uniq values in the container.
nuniq :: (Foldable f, Ord a) => f a -> Int
nuniq = S.size . foldr S.insert S.empty

-- | /O(n)/ Return the first item in a foldable.
head' :: Foldable f => f a -> a
head' = foldr1 (curry fst)

-- | /O(n)/ Return the last element of a foldable.
last' :: Foldable f => f a -> a
last' = foldr1 (curry snd)
