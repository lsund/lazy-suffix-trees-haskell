
module Algorithm.Ukkonen.Functional where

import           Protolude

import           Algorithm.Search
import           Data.Label       (Label (..))
import qualified Data.Label       as Label
import           Data.SuffixTree
import           Util


-- Update a tree
update :: (Ord a) => (STree a, Label a) -> (STree a, Label a)
update (tree, lbl@(Label mark l))
    | exists suffix tree = (tree, Label mark (succ l))
    | l == 0             = (insert (Label.empty lbl) tree, Label.tail lbl)
    | otherwise          = update (insert lbl tree, Label.shrink lbl)
    where suffix = take (succ l) mark


-------------------------------------------------------------------------------
-- Insert


addLeafEdge :: Ord a => Label a -> [Edge a] -> [Edge a]
addLeafEdge suffix [] = [leafEdge suffix]
addLeafEdge suffix (e : edges)
    | compareFirst suffix e == GT = e : addLeafEdge suffix edges
    | otherwise                   = leafEdge suffix : e : edges


splitEdge :: Ord a => Label a -> Edge a -> STree a -> (Edge a, Edge a)
splitEdge suffix edge tree
    | Label.compareFirst suffix' suffix'' == LT    = (sibling, newEdge)
    | otherwise                                    = (newEdge, sibling)
    where
        suffix'               = Label.drop suffix (_label edge)
        suffix''              = Label.drop suffix suffix
        sibling | isLeaf tree = Edge suffix'  (Leaf 0)
                | otherwise   = Edge (dropSuffixMark suffix edge) tree
        newEdge               = Edge suffix'' (Leaf 0)


splitAndInsert :: Ord a => Label a -> [Edge a] -> [Edge a]
splitAndInsert _ []                  = []
splitAndInsert suffix (edge : edges)
    | EQ /= compareFirst suffix edge = edge : splitAndInsert suffix edges
    | longer suffix edge             = edge { _subtree = tree' } : edges
    | otherwise                      = edge' : edges
    where
        tree' = insert (dropEdgeMark suffix edge) (_subtree edge)
        split = listify $ splitEdge suffix edge (_subtree edge)
        edge' = Edge (replaceMark suffix edge) (Branch split)

-- Function for inserting a suffix in a A+ tree. Let `sa` be the suffix to be
-- inserted and s' (suffix) be the path that emerges by following the mark s.
-- Distinguish between two cases:
--
-- (1) s' to be inserted does not end in a vertex.
-- (2) s' to be inserted does not end in a vertex.
insert :: Ord a => Label a -> STree a -> STree a
insert suffix (Branch edges')
    | Label.isEmpty suffix = Branch (addLeafEdge suffix edges')
    | otherwise            = Branch (splitAndInsert suffix edges')
insert _ _ = Leaf 0

naiveOnline :: Ord a => [a] -> STree a
naiveOnline x = fst (until stop update (Branch [], Label x 0))
    where
        stop (_, Label s l) = null $ drop l s
