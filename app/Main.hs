module Main where

import LazyTree (lazyAST)

import Protolude

main :: IO ()
main = print $ lazyAST "abcx" "xabxac"

