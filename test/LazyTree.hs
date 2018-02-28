
module LazyTree where

import           Prelude         (String)
import           Protolude
import           Test.QuickCheck

import Util

prop_removeHeads :: String -> [String] -> Bool
prop_removeHeads xs xss = (xs : [u | _: u <- xss]) == (xs : removeHeads xss)

prop_allStartsWith :: Char -> [String] -> Bool
prop_allStartsWith x xss =
    ([] == [0 | c : _ <- xss, x /= c]) == allStartsWith x xss

