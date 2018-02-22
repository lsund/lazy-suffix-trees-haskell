
module Algorithm.Ukkonen.Functional where

import           Data.List        (head, tail)
import           Protolude        hiding (head)

import           Algorithm.Search
import           Data.SuffixTree

label :: [a] -> Label a
label xs = (xs, length xs)


firstChar :: Edge a -> a
firstChar ((c : _, _), _) = c


update :: (Ord a) => (STree a, Label a) -> (STree a, Label a)
update (tree, (x : xs, l))
    | exists suffix tree = (tree, (x : xs, succ l))
    | 0 == l             = (tree', (xs, 0))
    | otherwise          = update (tree', (xs, pred l))
    where
        tree' = insert (x : xs, l) tree
        suffix = take (succ l) (x : xs)
update x = x

-- Insert a suffix
insert :: Ord a => Label a -> STree a -> STree a
insert (suff@(c : _), 0) (Branch edges) = Branch (insert' edges)
    where insert' [] = [(label suff, Leaf 0)]
          insert' (e : es)
            | c > firstChar e   = e : insert' es
            | otherwise         = (label suff, Leaf 0) : e : es

insert (suff@(c : _), l) (Branch edges) = Branch (insert' edges)
    where insert' (e@(cus@(mark, cl), v) : es')
            | c /= firstChar e                     = e : insert' es'
            | (not . isLeaf) v && l >= cl          = (cus, v') : es'
            | head x < head y                      = ((mark,l), Branch [ex,ey]) : es'
            | otherwise                            = ((mark,l),Branch [ey,ex]) : es'
            where
                v' = insert (drop cl suff,l - cl) v
                x = drop l mark
                y = drop l suff
                ex | isLeaf v = ((x,length x),Leaf 0)
                   |  otherwise = ((x, cl - l), v)
                ey = ((y,length y),Leaf 0)
          insert' _ = []

insert _ _ = Leaf 0

naiveOnline :: Ord a => [a] -> STree a
naiveOnline x = fst (until stop update (Branch [], (x, 0)))
    where
        stop (_, (s, l)) = [] == drop l s
