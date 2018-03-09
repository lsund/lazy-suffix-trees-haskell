
import Protolude
import Test.QuickCheck

import Ukkonen
import LazyTree
import Util

runTest = quickCheck prop_countsort

main :: IO ()
main = runTest
