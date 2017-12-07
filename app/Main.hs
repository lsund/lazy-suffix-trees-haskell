module Main where

import Lib (lazyAST)

import Protolude

main :: IO ()
main = print $ lazyAST "abcx" "xabxac"

