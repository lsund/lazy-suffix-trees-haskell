
module Data.SuffixTree where

import           Protolude

-------------------------------------------------------------------------------
-- Data

data Label2 a = Label2
    { _mark :: [a]
    , _len  :: Int
    } deriving (Eq, Show)

data Edge2 a = Edge2
    { _label   :: Label2 a
    , _subtree :: STree2 a
    } deriving (Eq, Show)


data STree2 a = Leaf2 { _leafNumber2 :: Int }
              | Branch2 { _branches2   :: [Edge2 a] }
              deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Old
type SuffixList a = [[a]]

type Alphabet a = [a]

type Label a = ([a], Int)

type Edge a = (Label a, STree a)


data STree a = Leaf     { _leafNumber :: Int }
             | Branch   { _branches   :: [Edge a] }
             deriving (Eq, Show)

type EdgeFunction a = SuffixList a -> (Int, SuffixList a)

isLeaf :: STree2 a -> Bool
isLeaf (Leaf2 _) = True
isLeaf _        = False


