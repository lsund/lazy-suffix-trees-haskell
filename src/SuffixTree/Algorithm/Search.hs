
module SuffixTree.Algorithm.Search where

import qualified Data.Text.Lazy                           as T
import           Protolude

import           SuffixTree.Data.Label
import           SuffixTree.Data.SuffixTree

type Pattern = T.Text

firstMatch :: [Edge] -> Pattern -> Maybe (Pattern, STree)
firstMatch []            _ = Nothing
firstMatch (Edge l t : xs) p =
    case match p (Edge l t) of
        Nothing -> firstMatch xs p
        Just x  -> Just x


match :: Pattern -> Edge -> Maybe (Pattern, STree)
match p (Edge (Label s len) t)
    | T.take len s `T.isPrefixOf` p             = Just (T.drop len p, t)
    | p            `T.isPrefixOf` T.take len s  = Just ("", t)
    | otherwise                                 = Nothing



search :: STree -> Pattern -> Maybe (Pattern, STree)
search (Leaf i)          p      = Just (p, Leaf i)
search (Branch [])       _      = Nothing
search (Branch branches) p =
    case firstMatch branches p of
        Just ("", t)   -> Just ("", t)
        Just (rest, t) -> search t rest
        Nothing        -> Nothing


indices :: Pattern -> STree -> Maybe [Int64]
indices p t = indices' . snd <$> search t p
    where
        indices' :: STree -> [Int64]
        indices' (Leaf i)    = [i]
        indices' (Branch xs) = sort $ concatMap (indices' . _subtree) xs


exists :: Pattern -> STree -> Bool
exists p t = isJust $ search t p
