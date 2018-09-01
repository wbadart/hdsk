{-|
Module:       Hdsk.NaiveBayes
Description:  Collection of bayesian methods
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

A simple implementation of a Naive Bayes classifier for purely categorical
data.
-}

module Hdsk.NaiveBayes
( CPT, mkTables, classify, posterior
, likelihood, labelCount, dataLength
) where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Type alias for the conditional probability table data structure. Maps
-- labels to lookup tables where lookup tables map from feature values to
-- the number of times that feature value was observed with the parent
-- label.
type CPT = Map String (Map String Int)

-- | /O(n * k * (log l + log m))/ where /n/ is the number of data
-- points, /k/ is the length of each data point, /l/ is the number of
-- labels, and /m/ is the gretest number of feature values for any
-- feature. Generate the conditional probability tables over the
-- dataset. Counts the occurences of each feature value given a class.
-- Maps from class label to a map from feature value to count.
mkTables :: [[String]] -> [CPT]
mkTables dataset = foldr updateTables initialTables dataset
  where updateTables tup = (zipWith $ incVal (last tup)) tup
        initialTables    = map (const M.empty) $ init (head dataset)

-- | /O(l k m)/ where /l/ is the number of labels, /k/ is the length of
-- each feature vector, and /m/ is the number of feature values. Given a
-- list of conditional probability tables and an unlabeled tuple,
-- predicts the label of the tuple.
classify :: [CPT] -> [String] -> String
classify [] _ = undefined
classify cpts@(cpt1:_) tup =
  maximumBy (compare `on` posterior cpts tup) $ M.keys cpt1

-- | /O(k m)/ where /k/ is the length of the feature vector and /m/ is
-- the number of feature values. Given conditional probability tables, a
-- tuple, and a label calculate the posterior probability of the label
-- given that tuple. /P(C|X)/
posterior :: [CPT] -> [String] -> String -> Double
posterior [] _ _ = undefined
posterior cpts@(cpt1:_) tup c =
    prior * product (zipWith (likelihood c) tup cpts)
  where prior = labelCount cpt1 c / dataLength cpt1  -- P(c)

-- | /O(m)/ Given a class, feature value, and conditional probability
-- table, compute the likelihood of that configuration. /P(x_i|c)/
likelihood :: String -> String -> CPT -> Double
likelihood c x cpt = fromIntegral ((cpt M.! c) M.! x) / labelCount cpt c


-- ===== Utilities ===== --

-- | /O(log l + log m)/ where /l/ is the number of labels and /m/ is the
-- number of feature values. Used for zipping together a data tuple and
-- the list of conditional probability tables.
incVal :: String -> String -> CPT -> CPT
incVal c val table = M.insert c byLabel table
  where byLabel = M.insertWith (+) val 1
                $ M.findWithDefault M.empty c table

-- | /O(m)/ where /m/ is the number of feature values. Computes the
-- frequency of the given label from the given conditional prob. table.
labelCount :: Num n => CPT -> String -> n
labelCount cpt c = sum $ map fromIntegral (M.elems (cpt M.! c))

-- | /O(l m)/ Calculate the number of training instances from a CPT.
dataLength :: Num n => CPT -> n
dataLength = fromIntegral . sum . map (sum . M.elems) . M.elems
