import           Test.QuickCheck

import qualified LazyTree.Functional as LT
import qualified Ukkonen.Functional  as U

prop_tree :: String -> String -> Bool
prop_tree a b = ta == tb
    where ta = LazyTree.toTree $ LazyTree.lazyAST a b
          tb = Other.LazyTree.toTree $ Other.LazyTree.lazyAST a b

runTest = quickCheck prop_tree
