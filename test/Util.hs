
module Util where

import           Data.List       as List
import           Prelude         (String)
import           Protolude
import           SuffixTree.Util
import           Test.QuickCheck

lcp :: (Eq a) => [[a]] -> [a]
lcp [] = []
lcp xs = ((List.head <$>) . takeWhile allEqual . truncTranspose) xs
  where
  -- Similar to transpose, but stops on end of shortest list.
    truncTranspose :: [[a]] -> [[a]]
    truncTranspose [] = []
    truncTranspose xs
      | any null xs = []
      | otherwise = (List.head <$> xs) : truncTranspose (List.tail <$> xs)
    allEqual :: (Eq a) => [a] -> Bool
    allEqual (x:xs) = all (== x) xs


prop_lcp :: [[String]] -> Bool
prop_lcp [[[]]] = True
prop_lcp [[]]   = True
prop_lcp []     = True
prop_lcp xs     = length (lcp xs) == longestCommonPrefix xs
