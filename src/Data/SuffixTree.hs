
module Data.SuffixTree where

import           Data.Label as Label
import           Protolude

-------------------------------------------------------------------------------
-- Data

data Edge a = Edge
    { _label   :: Label a
    , _subtree :: STree a
    } deriving (Eq, Show)


data STree a = Leaf { _leafNumber :: Int }
              | Branch { _branches   :: [Edge a] }
              deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Old
type SuffixList a = [[a]]

type Alphabet a = [a]

type EdgeFunction a = SuffixList a -> (Int, SuffixList a)

isLeaf :: STree a -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False

-------------------------------------------------------------------------------
-- Edge

compareFirst :: Ord a => Label a -> Edge a -> Ordering
compareFirst (Label (x : _) _) (Edge (Label (y : _) _) _) = x `compare` y

edgeChar :: Edge a -> a
edgeChar (Edge (Label (c : _) _) _) = c

leafEdge :: Label a -> Edge a
leafEdge suffix = Edge (Label.fromList (_mark suffix)) (Leaf 0)

isLeafEdge :: Edge a -> Bool
isLeafEdge = isLeaf . _subtree

longer :: Label a -> Edge a -> Bool
longer suffix edge = not (isLeafEdge edge) && _len suffix > (_len . _label) edge

matchPrefix :: Edge a -> Label a -> Label a
matchPrefix edge suffix =
    Label
        (drop ((_len . _label) edge) (_mark suffix))
        (_len suffix - (_len . _label) edge)
