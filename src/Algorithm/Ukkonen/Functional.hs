
module Algorithm.Ukkonen.Functional where

import           Data.List        (head, tail)
import           Protolude        hiding (head)

import           Algorithm.Search
import           Data.SuffixTree

label :: [a] -> Label2 a
label xs = Label2 xs (length xs)


edgeChar :: Edge2 a -> a
edgeChar (Edge2 (Label2 (c : _) _) _) = c

-- Update a tree
update :: (Ord a) => (STree2 a, Label2 a) -> (STree2 a, Label2 a)
update (tree, Label2 (x : xs) l)
    | exists suffix tree = (tree, Label2 (x : xs) (succ l))
    where suffix = take (succ l) (x : xs)
update (tree, Label2 (x : xs) 0) = (insert (Label2 (x : xs) 0) tree, Label2 xs 0)
update (tree, Label2 (x : xs) l) =
    update (insert (Label2 (x : xs) l) tree, Label2 xs (pred l))

-- Insert a suffix
insert :: Ord a => Label2 a -> STree2 a -> STree2 a
insert (Label2 suff@(c : _) 0) (Branch2 edges) = Branch2 (insert' edges)
    where insert' [] = [Edge2 (label suff) (Leaf2 0)]
          insert' (e : es)
            | c > edgeChar e   = e : insert' es
            | otherwise         = Edge2 (label suff) (Leaf2 0) : e : es

insert (Label2 suff@(c : _) l) (Branch2 edges) = Branch2 (insert' edges)
    where insert' (e@(Edge2 cus@(Label2 mark cl) v) : es')
            | c /= edgeChar e                     = e : insert' es'
            | (not . isLeaf) v && l >= cl          = Edge2 cus v' : es'
            | head x < head y                      =
                Edge2 (Label2 mark l) (Branch2 [ex, ey]) : es'
            | otherwise                            =
                Edge2 (Label2 mark l) (Branch2 [ey, ex]) : es'
            where
                v' = insert (Label2 (drop cl suff) (l - cl)) v
                x = drop l mark
                y = drop l suff
                ex | isLeaf v = Edge2 (Label2 x (length x)) (Leaf2 0)
                   |  otherwise = Edge2 (Label2 x (cl - l)) v
                ey = Edge2 (Label2 y (length y)) (Leaf2 0)
          insert' _ = []

insert _ _ = Leaf2 0

naiveOnline :: Ord a => [a] -> STree2 a
naiveOnline x = fst (until stop update (Branch2 [], Label2 x 0))
    where
        stop (_, Label2 s l) = [] == drop l s
