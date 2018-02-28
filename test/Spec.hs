
import Protolude    hiding ((.&.))
import Test.QuickCheck

import Ukkonen
import LazyTree

runTest = quickCheck $
            prop_removeHeads .&. prop_allStartsWith .&. prop_tree

main :: IO ()
main = runTest
