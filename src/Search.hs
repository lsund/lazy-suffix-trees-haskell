
module Search where

import Protolude

import LazyTree.Functional

type Pattern a = [a]


match :: Eq a => Pattern a -> Label a -> Maybe (Pattern a)
match p (s, len)
    | isPrefixOf (take len s) p = Just (drop len p)
    | otherwise      = Nothing

search :: Eq a => STree a -> Pattern a -> Bool
search Leaf              _      = False
search (Branch [])       _      = False
search (Branch ((l, _) : _)) p =
    case match p l of
        Nothing -> False
        Just _  -> True


