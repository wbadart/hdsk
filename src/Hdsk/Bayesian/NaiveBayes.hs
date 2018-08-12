{-|
Module:       Hdsk.Bayesian.NaiveBayes
Description:  Collection of bayesian methods
Copyright:    (c) Will Badart, 2018
License:      BSD-3-Clause

A simple implementation of a Naive Bayes classifier for purely categorical
data.
-}

module Hdsk.Bayesian.NaiveBayes
( classify
) where

import Data.Function (on)
import Data.List (genericLength, maximumBy, nub)

type Tuple = [String]
-- ^ Quick alias for list of strings; represents one data object.

classify :: [Tuple] -> Tuple -> String
-- ^ Calculate the most probable label of the given tuple given the dataset
-- (list of tuples). The label of a data object should be the last field.
classify dataset tup = maximumBy (compare `on` posterior dataset tup) labels
  where labels = nub $ map last dataset

posterior :: [Tuple] -> Tuple -> String -> Double
-- ^ Calculate the posterior probability of a class given an instance.
posterior dataset tup c = likelihood c * prior c
  where prior c      = countBy ((== c) . last) dataset / genericLength dataset
        likelihood c = product [prob c i x | (i, x) <- zip [0..] tup]
        prob c i x   = countBy (match c i) dataset
                        / countBy ((== c) . last) dataset
        match c i y  = y !! i == tup !! i && last y == c


-- ===== Utilities ===== -

countBy :: Num n => (a -> Bool) -> [a] -> n
countBy p = genericLength . filter p
