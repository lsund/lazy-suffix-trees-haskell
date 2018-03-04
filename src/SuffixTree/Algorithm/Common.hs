
module SuffixTree.Algorithm.Common where

import           Data.Text
import           Data.Tree
import           Protolude

import           SuffixTree.Data.Label
import           SuffixTree.Data.SuffixTree


unfoldEdge :: Edge -> (Label, [Edge])
unfoldEdge (Edge l (Branch xs))   = (l, xs)
unfoldEdge (Edge (Label m n) (Leaf i)) = (showLeaf, [])
    where
        showLeaf = Label (m `append` "<" `append` show i `append` ">" :: Text) (n + 3)


toTree :: STree -> Tree Label
toTree = unfoldTree unfoldEdge . rootEdge
    where
        rootEdge = Edge (Label "r" (1 :: Int))

