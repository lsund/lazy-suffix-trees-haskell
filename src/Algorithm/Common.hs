
module Algorithm.Common where

import           Data.Tree
import           Prelude         (String)
import           Protolude

import           Data.SuffixTree


unfoldEdge :: Edge2 Char -> (Label2 Char, [Edge2 Char])
unfoldEdge (Edge2 l (Branch2 xs))   = (l, xs)
unfoldEdge (Edge2 (Label2 m n) (Leaf2 i)) = (showLeaf, [])
    where
        showLeaf = Label2 (m ++ "<" ++ show i ++ ">" :: String) (n + 3)


toTree :: STree2 Char -> Tree (Label2 Char)
toTree = unfoldTree unfoldEdge . rootEdge
    where
        rootEdge = Edge2 (Label2 "r" (1 :: Int))

