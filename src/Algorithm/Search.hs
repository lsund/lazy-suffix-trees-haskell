
module Algorithm.Search where

import           Protolude

import           Algorithm.LazyTree.Functional
import           Data.Label                    hiding (take, drop)
import           Data.SuffixTree

type Pattern a = [a]

firstMatch :: Eq a => [Edge a] -> Pattern a -> Maybe (Pattern a, STree a)
firstMatch []            _ = Nothing
firstMatch (Edge l t : xs) p =
    case match p (Edge l t) of
        Nothing -> firstMatch xs p
        Just x  -> Just x


match :: Eq a => Pattern a -> Edge a -> Maybe (Pattern a, STree a)
match p (Edge (Label s len) t)
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


-- indices :: Eq a => Alphabet a -> [a] -> Pattern a -> Maybe [Int]
-- indices as x p =
--     let t = lazyCST as x
--     in indices' . snd <$> search t p
--     where
--         indices' :: STree a -> [Int]
--         indices' (Leaf i)    = [i]
--         indices' (Branch xs) = sort $ concatMap (indices' . snd) xs


exists :: Eq a => Pattern a -> STree a -> Bool
exists p t = isJust $ search t p
