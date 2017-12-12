
module Test.Benchmark where

import Protolude
import Draw
import Data.Tree                (flatten)


import qualified Other.LazyTree (lazyAST, lazyCST, toTree)
import qualified LazyTree       (lazyAST, lazyCST, toTree)
import Util

alpha = ['A'..'Z']

-- TODO
