
module SuffixTree.Data.SuffixTree where

import           Data.Text             as Text
import           Protolude             hiding (head)

import           SuffixTree.Data.Label as Label

-------------------------------------------------------------------------------
-- Data

data Edge = Edge
    { _label   :: Label
    , _subtree :: STree
    } deriving (Eq, Show)


data STree      = Leaf      { _leafNumber :: Int }
                | Branch    { _branches   :: ![Edge] }
                deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Old
type SuffixList = [Text]

type Alphabet a = [a]

type EdgeFunction = SuffixList -> (Int, SuffixList)

isLeaf :: STree -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False


-------------------------------------------------------------------------------
-- Edge


compareFirst :: Label -> Edge -> Ordering
compareFirst (Label xs _) (Edge (Label ys _) _) = head xs `compare` head ys


edgeChar :: Edge -> Char
edgeChar (Edge (Label cs _) _) = head cs


leafEdge :: Int -> Label -> Edge
leafEdge l suffix = Edge (Label.fromList (_mark suffix)) (Leaf l)


isLeafEdge :: Edge -> Bool
isLeafEdge = isLeaf . _subtree


longer :: Label -> Edge -> Bool
longer suffix edge = not (isLeafEdge edge) && _len suffix > (_len . _label) edge


dropEdgeMark :: Label -> Edge -> Label
dropEdgeMark suffix edge =
    Label
        (Text.drop ((_len . _label) edge) (_mark suffix))
        (_len suffix - (_len . _label) edge)


dropSuffixMark :: Label -> Edge -> Label
dropSuffixMark suffix edge =
    Label
        (Text.drop (_len suffix) ((_mark . _label) edge))
        ((_len . _label) edge - _len suffix)


replaceMark :: Label -> Edge -> Label
replaceMark suffix edge =  Label.take (_len suffix) (_label edge)
