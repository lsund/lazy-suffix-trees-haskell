
module Data.SuffixTree where

import           Data.Label
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
