
module Algorithm.Search where

import           Protolude

import           Algorithm.LazyTree.Functional
import           Data.SuffixTree

type Pattern a = [a]

firstMatch :: Eq a => [Edge2 a] -> Pattern a -> Maybe (Pattern a, STree2 a)
firstMatch []            _ = Nothing
firstMatch (Edge2 l t : xs) p =
    case match p (Edge2 l t) of
        Nothing -> firstMatch xs p
        Just x  -> Just x


match :: Eq a => Pattern a -> Edge2 a -> Maybe (Pattern a, STree2 a)
match p (Edge2 (Label2 s len) t)
    | take len s `isPrefixOf` p          = Just (drop len p, t)
    | p          `isPrefixOf` take len s = Just ([], t)
    | otherwise                          = Nothing



search :: Eq a => STree2 a -> Pattern a -> Maybe (Pattern a, STree2 a)
search (Leaf2 i)          p      = Just (p, Leaf2 i)
search (Branch2 [])       _      = Nothing
search (Branch2 branches) p =
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


exists :: Eq a => Pattern a -> STree2 a -> Bool
exists p t = isJust $ search t p
