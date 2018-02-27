
module Algorithm.Ukkonen.Functional where

import           Data.List        (head)
import           Protolude        hiding (head)

import           Algorithm.Search
import qualified Data.Label       as Label
import           Data.Label       (Label(..))
import           Data.SuffixTree
import           Util

edgeChar :: Edge a -> a
edgeChar (Edge (Label (c : _) _) _) = c

matchPrefix :: Label a -> Label a -> Label a
matchPrefix lbl suffix = Label (drop (_len lbl) (_mark suffix)) (_len suffix - _len lbl)

-- Update a tree
update :: (Ord a) => (STree a, Label a) -> (STree a, Label a)
update (tree, Label (x : xs) l)
    | exists suffix tree = (tree, Label (x : xs) (succ l))
    where suffix = take (succ l) (x : xs)
update (tree, Label (x : xs) 0) = (insert (Label (x : xs) 0) tree, Label xs 0)
update (tree, Label (x : xs) l) =
    update (insert (Label (x : xs) l) tree, Label xs (pred l))
update(tree, lbl) = (tree, lbl)

-- longer : suffix -> edge label -> current tree -> bool
longer :: Label a -> Label a -> STree a -> Bool
longer suffix lbl tree = (not . isLeaf) tree && _len suffix > _len lbl


-- split : suffix -> edge label -> tree -> (edge, edge)
splitEdge :: Ord a => Label a -> Label a -> STree a -> (Edge a, Edge a)
splitEdge suffix lbl tree =
    let
        x = drop (_len suffix) (_mark lbl)
        y = drop (_len suffix) (_mark suffix)
        ex | isLeaf tree = Edge (Label x (length x)) (Leaf 0)
           | otherwise = Edge (Label x (_len lbl - _len suffix)) tree
        ey = Edge (Label y (length y)) (Leaf 0)
    in
        if head x < head y then (ex, ey) else (ey, ex)

compareFirst :: Eq a => Label a -> Edge a -> Bool
compareFirst (Label (x : _) _) (Edge (Label (y : _) _) _) = x == y

-- Insert a suffix
-- /s ends on a vertex
insert :: Ord a => Label a -> STree a -> STree a
insert (Label suff@(c : _) 0) (Branch edges) = Branch (insert' edges)
    where insert' [] = [Edge (Label.full suff) (Leaf 0)]
          insert' (e : es)
            | c > edgeChar e = e : insert' es
            | otherwise      = Edge (Label.full suff) (Leaf 0) : e : es

-- /s does not end on a vertex
insert suffix (Branch edges) = Branch (insert' edges)
    where insert' (e@(Edge lbl _) : es)
            | not $ compareFirst suffix e = e : insert' es
            | longer suffix lbl (_subtree e) = Edge lbl tree' : es
            | otherwise  = Edge (Label.take (_len suffix) lbl) (Branch split) : es
            where
                tree' = insert (matchPrefix lbl suffix) (_subtree e)
                split = listify $ splitEdge suffix lbl (_subtree e)
          insert' _ = []

insert _ _ = Leaf 0

naiveOnline :: Ord a => [a] -> STree a
naiveOnline x = fst (until stop update (Branch [], Label x 0))
    where
        stop (_, Label s l) = [] == drop l s
