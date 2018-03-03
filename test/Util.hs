
module Util where

import           Data.List                                as List
import           Prelude                                  (String)
import           Protolude
import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Util
import           Test.QuickCheck

prop_lcp :: [[String]] -> Bool
prop_lcp xs     = edgeCST xs == longestCommonPrefix xs
