
module Algorithm.Common where

import           Data.Tree
import           Prelude         (String)
import           Protolude

import           Data.SuffixTree


unfoldEdge :: Edge Char -> (Label Char, [Edge Char])
unfoldEdge (l, Branch xs)   = (l, xs)
unfoldEdge ((m, n), Leaf i) = (showLeaf, [])
    where
        showLeaf = (m ++ "<" ++ show i ++ ">" :: String, n + 3)


toTree :: STree Char -> Tree (Label Char)
toTree = unfoldTree unfoldEdge . rootEdge
    where
        rootEdge st = (("r", 1 :: Int), st)

