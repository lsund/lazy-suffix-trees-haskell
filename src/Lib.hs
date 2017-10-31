
module Lib where

import Protolude hiding (tails)

import Data.Text        (tails)
import Data.List        (init)

type Edge = (Text, SuffixTree)

data SuffixTree
    = Node [Edge]
    | Leaf Int
    deriving (Show)

suffixes :: Text -> [Text]
suffixes = init . tails

tree1 :: Text -> SuffixTree
tree1 t =
   let ss = suffixes t
   in Node [(t, Leaf 1)]

match :: Text -> Edge -> Bool
match = undefined -- TODO continue here

longestPath :: Text -> SuffixTree -> Text
longestPath = undefined

next :: SuffixTree -> [Text] -> SuffixTree
next = undefined

make :: Text -> SuffixTree
make = undefined

query :: Text -> Maybe [Int]
query = undefined
