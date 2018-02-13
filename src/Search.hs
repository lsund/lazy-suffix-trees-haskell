
module Search where

import Protolude

import Data.SuffixTree
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
    | take len s `isPrefixOf` p          = Just (drop len p, t)
    | p          `isPrefixOf` take len s = Just ([], t)
    | otherwise                          = Nothing



search :: Eq a => STree a -> Pattern a -> Maybe (Pattern a, STree a)
search (Leaf i)          p      = Just (p, Leaf i)
search (Branch [])       _      = Nothing
search (Branch branches) p =
    case firstMatch branches p of
        Just ([], t)   -> Just ([], t)
        Just (rest, t) -> search t rest
        Nothing        -> Nothing


indices :: Eq a => Alphabet a -> [a] -> Pattern a -> Maybe [Int]
indices as x p =
    let t = lazyCST as x
    in indices' . snd <$> search t p
    where
        indices' :: STree a -> [Int]
        indices' (Leaf i)    = [i]
        indices' (Branch xs) = sort $ concatMap (indices' . snd) xs


exists :: Eq a => Alphabet a -> [a] -> Pattern a -> Bool
exists as x p =
    let t = lazyCST as x
    in isJust $ search t p
