
module SuffixTree.Algorithm.Common where

import           Data.Tree
import           Prelude         (String)
import           Protolude

import           SuffixTree.Data.Label
import           SuffixTree.Data.SuffixTree


unfoldEdge :: Edge Char -> (Label Char, [Edge Char])
unfoldEdge (Edge l (Branch xs))   = (l, xs)
unfoldEdge (Edge (Label m n) (Leaf i)) = (showLeaf, [])
    where
        showLeaf = Label (m ++ "<" ++ show i ++ ">" :: String) (n + 3)


toTree :: STree Char -> Tree (Label Char)
toTree = unfoldTree unfoldEdge . rootEdge
    where
        rootEdge = Edge (Label "r" (1 :: Int))

