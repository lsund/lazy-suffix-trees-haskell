
module Util where

import           Data.List       (intercalate)
import           Prelude         (String)
import           Protolude
import           Test.QuickCheck
import           SuffixTree.Util

lcp :: (Eq a) => [[a]] -> [a]
lcp = (head <$>) . takeWhile allEqual . truncTranspose
  where
  -- Similar to transpose, but stops on end of shortest list.
    truncTranspose :: [[a]] -> [[a]]
    truncTranspose xs
      | any null xs = []
      | otherwise = (head <$> xs) : truncTranspose (tail <$> xs)
    allEqual
      :: (Eq a)
      => [a] -> Bool
    allEqual (x:xs) = all (== x) xs

prop_lcp :: [[String]] -> Bool
prop_lcp xs = length (lcp xs) == longestCommonPrefix xs
