
module Algorithm.Ukkonen.Functional where

import           Data.List        (head, tail, (!!))
import           Protolude        hiding (head)

import           Algorithm.Search
import           Data.SuffixTree


update :: (Ord a) => (STree a, Label a) -> (STree a, Label a)
update (root,(s,slen))
    | exists (take (succ slen) s) root  = (root,(s,succ slen))
    | 0 == slen                         = (root',(tail s,0))
    | otherwise                         = update (root', (tail s, pred slen))
    where
        root' = insRelSuff (s,slen) root


insRelSuff :: Ord a => Label a -> STree a -> STree a
insRelSuff (aw@(a:_), 0) (Branch es) = Branch (g es)
    where g [] = [((aw,length aw), Leaf 0)]
          g (cusn@((c:_,_),_):es')
            | a > c = cusn:g es'
            | otherwise = ((aw,length aw), Leaf 0):cusn:es'
          g _ = []
insRelSuff (aw@(a:_),slen) (Branch es) = Branch (g es)
    where g (cusn@(cus@(cu@(c:_),culen),node):es')
            | a /= c                        = cusn:g es'
            | (not . isLeaf) node && slen >= culen = (cus,node'):es'
            | head x < head y = ((cu,slen),Branch [ex,ey]):es'
            | otherwise       = ((cu,slen),Branch [ey,ex]):es'
            where
                node' = insRelSuff (drop culen aw,slen-culen) node
                x = drop slen cu
                y = drop slen aw
                ex | isLeaf node = ((x,length x),Leaf 0)
                   |  otherwise = ((x,culen-slen),node)
                ey = ((y,length y),Leaf 0)
          g _ = []
insRelSuff _ _ = Leaf 0

naiveOnline :: Ord a => [a] -> STree a
naiveOnline x = fst (until stop update (Branch [], (x, 0)))
    where
        stop (_, (s, slen)) = [] == drop slen s
