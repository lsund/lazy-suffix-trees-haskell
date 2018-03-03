
import Protolude    hiding ((.&.))
import Test.QuickCheck

import Ukkonen
import LazyTree
import Util

runTest = quickCheck $
            prop_removeHeads .&. prop_allStartsWith .&. prop_tree .&. prop_lcp

main :: IO ()
main = runTest
