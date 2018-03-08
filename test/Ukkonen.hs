module Ukkonen where

import           Data.List
import           Data.Text.Lazy                           as T
import           Prelude                                  (String)
import           Protolude
import           Test.QuickCheck

import           SuffixTree.Algorithm.Common
import qualified SuffixTree.Algorithm.LazyTree.Functional as LT
import qualified SuffixTree.Algorithm.Ukkonen.Functional  as U

prop_tree :: T.Text -> Bool
prop_tree x = ta == tb
    where ta = toTree $(LT.lazyTree LT.edgeCST) x
          tb = toTree $ U.ukkonen x

