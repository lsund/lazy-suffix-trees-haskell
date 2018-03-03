
import Protolude    hiding ((.&.))
import Test.QuickCheck

import Ukkonen
import LazyTree
import Util

-- prop tree does not work since the branch order is different in ukkonen and
-- lazytree
runTest = quickCheck $
            prop_removeHeads .&. prop_allStartsWith .&. prop_lcp

main :: IO ()
main = runTest
