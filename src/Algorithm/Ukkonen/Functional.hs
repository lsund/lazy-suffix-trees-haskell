
module Algorithm.Ukkonen.Functional where

import           Protolude hiding (empty)

import           Algorithm.Search
import           Data.Label       (Label (..), grow, shrink, empty)
import qualified Data.Label       as Label
import           Data.SuffixTree
import           Util


-------------------------------------------------------------------------------
-- Insert


addLeafEdge :: Ord a => Int -> Label a -> [Edge a] -> [Edge a]
addLeafEdge l suffix [] = [leafEdge l suffix]
addLeafEdge l suffix (e : edges)
    | compareFirst suffix e == GT = e : addLeafEdge (traceShowId l) suffix edges
    | otherwise                   = leafEdge (traceShowId l) suffix : e : edges


splitEdge :: Ord a => Int -> Label a -> Edge a -> STree a -> (Edge a, Edge a)
splitEdge l suffix edge tree
    | Label.compareFirst suffix' suffix'' == LT    = (sibling, newEdge)
    | otherwise                                    = (newEdge, sibling)
    where
        suffix'               = Label.drop suffix (_label edge)
        suffix''              = Label.drop suffix suffix
        sibling | isLeaf tree = Edge suffix' tree
                | otherwise   = Edge (dropSuffixMark suffix edge) tree
        newEdge               = Edge suffix'' (Leaf l)


splitAndInsert :: Ord a => Int -> Label a -> [Edge a] -> [Edge a]
splitAndInsert _ _ []                  = []
splitAndInsert l suffix (edge : edges)
    | EQ /= compareFirst suffix edge = edge : splitAndInsert l suffix edges
    | longer suffix edge             = edge { _subtree = tree' } : edges
    | otherwise                      = edge' : edges
    where
        tree' = insert l (dropEdgeMark suffix edge) (_subtree edge)
        split = listify $ splitEdge l suffix edge (_subtree edge)
        edge' = Edge (replaceMark suffix edge) (Branch split)

-- Function for inserting a suffix in a A+ tree. Let `sa` be the suffix to be
-- inserted and s' (suffix) be the path that emerges by following the mark s.
-- Distinguish between two cases:
--
-- (1) s' to be inserted does not end in a vertex.
-- (2) s' to be inserted does not end in a vertex.
insert :: Ord a => Int -> Label a -> STree a -> STree a
insert l suffix (Branch edges')
    | Label.isEmpty suffix = Branch (addLeafEdge l suffix edges')
    | otherwise            = Branch (splitAndInsert l suffix edges')
insert _ _ _ = Leaf 0


-- Update a tree
update :: (Ord a) => (STree a, Label a, Int) -> (STree a, Label a, Int)
update (tree, lbl@(Label mark len), l)
    | exists suffix tree = (tree, grow lbl, l)
    | len == 0           = (insert l (empty lbl) tree, Label.tail lbl, succ l)
    | otherwise          = update (insert l lbl tree, shrink lbl, succ l)
    where suffix = take (succ len) mark


naiveOnline :: Ord a => [a] -> STree a
naiveOnline x = fst3 (until stop update (Branch [], Label x 0, 0))
    where
        stop (_, Label s l, _) = null $ drop l s
