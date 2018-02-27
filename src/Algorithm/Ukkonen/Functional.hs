
module Algorithm.Ukkonen.Functional where

import           Data.List        (head, tail)
import           Protolude        hiding (head)

import           Algorithm.Search
import           Data.SuffixTree

label :: [a] -> Label a
label xs = Label xs (length xs)


edgeChar :: Edge a -> a
edgeChar (Edge (Label (c : _) _) _) = c

-- Update a tree
update :: (Ord a) => (STree a, Label a) -> (STree a, Label a)
update (tree, Label (x : xs) l)
    | exists suffix tree = (tree, Label (x : xs) (succ l))
    where suffix = take (succ l) (x : xs)
update (tree, Label (x : xs) 0) = (insert (Label (x : xs) 0) tree, Label xs 0)
update (tree, Label (x : xs) l) =
    update (insert (Label (x : xs) l) tree, Label xs (pred l))

-- Insert a suffix
insert :: Ord a => Label a -> STree a -> STree a
insert (Label suff@(c : _) 0) (Branch edges) = Branch (insert' edges)
    where insert' [] = [Edge (label suff) (Leaf 0)]
          insert' (e : es)
            | c > edgeChar e   = e : insert' es
            | otherwise         = Edge (label suff) (Leaf 0) : e : es

insert (Label suff@(c : _) l) (Branch edges) = Branch (insert' edges)
    where insert' (e@(Edge cus@(Label mark cl) v) : es')
            | c /= edgeChar e                     = e : insert' es'
            | (not . isLeaf) v && l >= cl          = Edge cus v' : es'
            | head x < head y                      =
                Edge (Label mark l) (Branch [ex, ey]) : es'
            | otherwise                            =
                Edge (Label mark l) (Branch [ey, ex]) : es'
            where
                v' = insert (Label (drop cl suff) (l - cl)) v
                x = drop l mark
                y = drop l suff
                ex | isLeaf v = Edge (Label x (length x)) (Leaf 0)
                   |  otherwise = Edge (Label x (cl - l)) v
                ey = Edge (Label y (length y)) (Leaf 0)
          insert' _ = []

insert _ _ = Leaf 0

naiveOnline :: Ord a => [a] -> STree a
naiveOnline x = fst (until stop update (Branch [], Label x 0))
    where
        stop (_, Label s l) = [] == drop l s
