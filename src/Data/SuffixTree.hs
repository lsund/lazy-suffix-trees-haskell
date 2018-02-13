
module Data.SuffixTree where

import Protolude

-------------------------------------------------------------------------------
-- Data

type SuffixList a = [[a]]

type Label a = ([a], Int)

type Alphabet a = [a]

type Edge a = (Label a, STree a)

data STree a = Leaf     { _leafNumber :: Int }
             | Branch   { _branches   :: [Edge a] }
             deriving (Eq, Show)

type EdgeFunction a = SuffixList a -> (Int, SuffixList a)

isLeaf :: STree a -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False


