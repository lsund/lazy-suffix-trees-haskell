module Ukkonen where

import           Test.QuickCheck

import           Algorithm.Common
import qualified Algorithm.LazyTree.Functional as LT
import qualified Algorithm.Ukkonen.Functional  as U
import           Data.List
import           Prelude                       (String)
import           Protolude

prop_tree :: String -> Bool
prop_tree x = ta == tb
    where ta = toTree $ LT.lazyCST (nub x) x
          tb = toTree $ U.ukkonen x

