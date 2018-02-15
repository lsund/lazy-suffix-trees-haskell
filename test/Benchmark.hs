
module Test.Benchmark where

import           Data.Tree      (flatten)
import           Draw
import           Protolude


import qualified LazyTree       (lazyAST, lazyCST, toTree)
import qualified Other.LazyTree (lazyAST, lazyCST, toTree)
import           Util

alpha = ['A'..'Z']

-- TODO
