
module Lib where

import Protolude hiding (tails)

-- import Data.Maybe       (fromJust)
-- import Data.Text        (tails, commonPrefixes)
import Data.List        (groupBy)

-- After Giegerich and Kurtz 1994

-- A suffixtree is eather a leaf or a branch with a number of subtrees.
-- A subtree is the product of a label and a suffixtree.
data SuffixTree a = Leaf | Branch [([a], SuffixTree a)] deriving (Show)

type EdgeFunction a  = [[a]] -> ([a], [[a]])

edgePst :: EdgeFunction a
edgePst [s] = (s, [[]])
edgePst ss  = ([], ss)


suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x : xs) = (x : xs) : suffixes xs


groupByHead :: Ord a => [[a]] -> [[[a]]]
groupByHead = groupBy compFun . sortBy ordFun
  where
    ordFun []      []      = EQ
    ordFun (x : _) (y : _) = x `compare` y
    ordFun []      _       = LT
    ordFun _      []       = GT
    compFun xs             = (== EQ) . ordFun xs


-- construct _ [[]] = Leaf
-- construct [[]]      = Leaf
-- construct suffixes  =
--     let
--         -- [[first letter], [second letter]...]
--         grouped = groupByHead suffixes
--         firsts = map (\((x : _) : _) -> x) grouped
--         -- [([prefix], [[
--         prefixed = zip firsts $ map edgePst grouped
--     in
--       Branch $ map (\(a, (p, y : ys)) -> (a : p, construct ys)) prefixed


-- make t = construct $ suffixes t


-- Choose a common prefix for each group for the edge label
-- Shoul
-- lazyTree :: EdgeFunction a -> [a] -> [a] -> SuffixTree a
-- lazyTree mkEdge letters t =
--     construct mkEdge letters $ suffixes t

-- lazyPst :: [a] -> [a] -> SuffixTree a
-- lazyPst = lazyTree edgePst
