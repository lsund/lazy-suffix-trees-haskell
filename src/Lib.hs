
module Lib where

import Protolude hiding (tails)

import Data.Maybe       (fromJust)
import Data.Text        (tails, stripPrefix, commonPrefixes)
import Data.List        (init)

type Edge = (Text, SuffixTree)

data SuffixTree
    = Node [Edge]
    | Leaf Int
    deriving (Show)

suffixes :: Text -> [Text]
suffixes = init . tails

tree1 :: Text -> SuffixTree
tree1 text =
   let ss = suffixes text
   in Node [(text, Leaf 1)]

match :: Text -> Edge -> Maybe (Text, Text, Text)
match text (label, _) = commonPrefixes text label

extend :: Int -> Text -> SuffixTree -> SuffixTree
extend n "" tree = tree
extend n text tree@(Node es) =
    case match text (fromJust $ head es) of
        Just (p, s1, s2) -> undefined -- TODO continue here
        Nothing -> undefined

next :: SuffixTree -> [Text] -> SuffixTree
next = undefined

make :: Text -> SuffixTree
make = undefined

query :: Text -> Maybe [Int]
query = undefined
