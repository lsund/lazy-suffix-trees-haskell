
module Search where

import Protolude

import LazyTree.Functional

type Pattern a = [a]

firstMatch :: Eq a => [(Label a, STree a)] -> Pattern a -> Maybe (Pattern a, STree a)
firstMatch []            _ = Nothing
firstMatch ((l, t) : xs) p =
    case match p (l, t) of
        Nothing         -> firstMatch xs p
        Just x          -> Just x


match :: Eq a => Pattern a -> (Label a, STree a) -> Maybe (Pattern a, STree a)
match p ((s, len), t)
    | isPrefixOf (take len s) p = Just (drop len p, t)
    | isPrefixOf p (take len s) = Just ([], t)
    | otherwise                 = Nothing

search :: Eq a => STree a -> Pattern a -> Maybe (Pattern a, STree a)
search (Leaf i)          p      = Just (p, Leaf i)
search (Branch [])       _      = Nothing
search (Branch branches) p =
    case firstMatch branches p of
        Just ([], t)   -> Just ([], t)
        Just (rest, t) -> search t rest
        Nothing -> Nothing


