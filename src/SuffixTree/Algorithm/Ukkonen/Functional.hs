
module SuffixTree.Algorithm.Ukkonen.Functional where

import           Data.Text                   hiding (empty)
import           Protolude                   hiding (drop, empty, null, take)

import           SuffixTree.Algorithm.Search
import           SuffixTree.Data.Label       (Label (..), empty, grow, isEmpty,
                                              shrink)
import qualified SuffixTree.Data.Label       as Label
import           SuffixTree.Data.SuffixTree  as Edge
import           SuffixTree.Util


-------------------------------------------------------------------------------
-- Insert


addLeafEdge :: (Label, Int) -> [Edge] -> [Edge]
addLeafEdge (suffix, l) []          = [leafEdge l suffix]
addLeafEdge (suffix, l) (e : edges)
    | Edge.compareFirst suffix e == GT = e : addLeafEdge (suffix, l) edges
    | otherwise                        = leafEdge l suffix : e : edges


splitEdge :: (Label, Int) -> Edge -> STree -> (Edge, Edge)
splitEdge (suffix, l) edge tree
    | Label.compareFirst suffix' suffix'' == LT    = (sibling, newEdge)
    | otherwise                                    = (newEdge, sibling)
    where
        suffix'               = Label.drop suffix (_label edge)
        suffix''              = Label.drop suffix suffix
        sibling | isLeaf tree = Edge suffix' tree
                | otherwise   = Edge (dropSuffixMark suffix edge) tree
        newEdge               = Edge suffix'' (Leaf l)


splitAndInsert :: (Label, Int) -> [Edge] -> [Edge]
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
insert :: (Label, Int) -> STree -> STree
insert (suffix, l) (Branch edges')
    | isEmpty suffix = Branch (addLeafEdge (suffix, l) edges')
    | otherwise            = Branch (splitAndInsert (suffix, l) edges')
insert _ _ = Leaf 0


-- Update a tree
update :: (STree, (Label, Int)) -> (STree, (Label, Int))
update (tree, (lbl@(Label mark len), l))
    | exists suffix tree = (tree, (grow lbl, l))
    | len == 0           = (insert (empty lbl, l) tree, (Label.tail lbl, succ l))
    | otherwise          = update (insert (lbl, l) tree, (shrink lbl, succ l))
    where suffix = take (succ len) mark


ukkonen :: Text -> STree
ukkonen x = fst (until stop update (Branch [], (Label x 0, 0)))
    where
        stop (_, (Label s l, _)) = null $ drop l s
