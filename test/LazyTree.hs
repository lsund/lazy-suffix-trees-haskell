
module Test.LazyTree where

import Prelude (String)
import Protolude
import Test.QuickCheck

import qualified Other.LazyTree (lazyAST, toTree)
import qualified LazyTree       (lazyAST, toTree)

prop_tree :: String -> String -> Bool
prop_tree a b = ta == tb
    where ta = LazyTree.toTree $ LazyTree.lazyAST a b
          tb = Other.LazyTree.toTree $ Other.LazyTree.lazyAST a b


runTest = quickCheck prop_tree
