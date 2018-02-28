
module Algorithm.Ukkonen.Functional where

import           Protolude        hiding (empty)

import           Algorithm.Search
import           Data.Label       (Label (..), empty, grow, isEmpty, shrink)
import qualified Data.Label       as Label
import           Data.SuffixTree  as Edge
import           Util


-------------------------------------------------------------------------------
-- Insert


addLeafEdge :: Ord a => (Label a, Int) -> [Edge a] -> [Edge a]
addLeafEdge (suffix, l) []          = [leafEdge l suffix]
addLeafEdge (suffix, l) (e : edges)
    | Edge.compareFirst suffix e == GT = e : addLeafEdge (suffix, l) edges
    | otherwise                        = leafEdge l suffix : e : edges


splitEdge :: Ord a => (Label a, Int) -> Edge a -> STree a -> (Edge a, Edge a)
splitEdge (suffix, l) edge tree
    | Label.compareFirst suffix' suffix'' == LT    = (sibling, newEdge)
    | otherwise                                    = (newEdge, sibling)
    where
        suffix'               = Label.drop suffix (_label edge)
        suffix''              = Label.drop suffix suffix
        sibling | isLeaf tree = Edge suffix' tree
                | otherwise   = Edge (dropSuffixMark suffix edge) tree
        newEdge               = Edge suffix'' (Leaf l)


splitAndInsert :: Ord a => (Label a, Int) -> [Edge a] -> [Edge a]
splitAndInsert _ []                  = []
splitAndInsert (suffix, l) (edge : edges)
    | EQ /= Edge.compareFirst suffix edge = edge : splitAndInsert (suffix, l) edges
    | longer suffix edge             = edge { _subtree = tree' } : edges
    | otherwise                      = edge' : edges
    where
        tree' = insert (dropEdgeMark suffix edge, l) (_subtree edge)
        split = listify $ splitEdge (suffix, l) edge (_subtree edge)
        edge' = Edge (replaceMark suffix edge) (Branch split)


-- Function for inserting a suffix in a A+ tree. Let `sa` be the suffix to be
-- inserted and s' (suffix) be the path that emerges by following the mark s.
-- Distinguish between two cases:
--
-- (1) s' to be inserted does not end in a vertex.
-- (2) s' to be inserted does not end in a vertex.
insert :: Ord a => (Label a, Int) -> STree a -> STree a
insert (suffix, l) (Branch edges')
    | isEmpty suffix = Branch (addLeafEdge (suffix, l) edges')
    | otherwise            = Branch (splitAndInsert (suffix, l) edges')
insert _ _ = Leaf 0


-- Update a tree
update :: (Ord a) => (STree a, (Label a, Int)) -> (STree a, (Label a, Int))
update (tree, (lbl@(Label mark len), l))
    | exists suffix tree = (tree, (grow lbl, l))
    | len == 0           = (insert (empty lbl, l) tree, (Label.tail lbl, succ l))
    | otherwise          = update (insert (lbl, l) tree, (shrink lbl, succ l))
    where suffix = take (succ len) mark


ukkonen :: Ord a => [a] -> STree a
ukkonen x = fst (until stop update (Branch [], (Label x 0, 0)))
    where
        stop (_, (Label s l, _)) = null $ drop l s
