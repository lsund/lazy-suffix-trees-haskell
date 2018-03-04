
module SuffixTree.Algorithm.Search where

import           Data.Text hiding (concatMap)
import           Protolude hiding (isPrefixOf, take, drop)

import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Data.Label                    hiding (drop, take)
import           SuffixTree.Data.SuffixTree

type Pattern = Text

firstMatch :: [Edge] -> Pattern -> Maybe (Pattern, STree)
firstMatch []            _ = Nothing
firstMatch (Edge l t : xs) p =
    case match p (Edge l t) of
        Nothing -> firstMatch xs p
        Just x  -> Just x


match :: Pattern -> Edge -> Maybe (Pattern, STree)
match p (Edge (Label s len) t)
    | take len s `isPrefixOf` p          = Just (drop len p, t)
    | p          `isPrefixOf` take len s = Just ("", t)
    | otherwise                          = Nothing



search :: STree -> Pattern -> Maybe (Pattern, STree)
search (Leaf i)          p      = Just (p, Leaf i)
search (Branch [])       _      = Nothing
search (Branch branches) p =
    case firstMatch branches p of
        Just ("", t)   -> Just ("", t)
        Just (rest, t) -> search t rest
        Nothing        -> Nothing


indices :: Pattern -> STree -> Maybe [Int]
indices p t = indices' . snd <$> search t p
    where
        indices' :: STree -> [Int]
        indices' (Leaf i)    = [i]
        indices' (Branch xs) = sort $ concatMap (indices' . _subtree) xs


exists :: Pattern -> STree -> Bool
exists p t = isJust $ search t p
