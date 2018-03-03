module Ukkonen where

import           Data.List
import           Prelude                                  (String)
import           Protolude
import           Test.QuickCheck

import           SuffixTree.Algorithm.Common
import qualified SuffixTree.Algorithm.LazyTree.Functional as LT
import qualified SuffixTree.Algorithm.Ukkonen.Functional  as U

prop_tree :: String -> Bool
prop_tree x = ta == tb
    where ta = toTree $ LT.lazyCST (nub x) x
          tb = toTree $ U.ukkonen x

