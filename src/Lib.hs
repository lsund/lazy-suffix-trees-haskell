
module Lib where

import Protolude

type Label a = ([a], Int)
type EdgeFunction a = [[a]] -> (Int, [[a]])

data STree a = Leaf | Branch [(Label a, STree a)] deriving (Eq, Show)

-------------------------------------------------------------------------
-- Impl

lazyTree :: Eq a => EdgeFunction a -> [a] -> [a] -> STree a
lazyTree edge alpha t = sTr $ suffixes t
    where sTr [[]] = Leaf
          sTr ss   =
            Branch
                [((a : sa, succ cpl), sTr ssr) | a <- alpha
                                               , sa : ssa <- [select ss a]
                                               , (cpl, ssr) <- [edge (sa : ssa)]]


-- select suffixes strating with a
select :: Eq a => [[a]] -> a -> [[a]]
select ss a = [u | c : u <- ss, a == c]


-- All non-empty suffixes
suffixes :: [a] -> [[a]]
suffixes []         = []
suffixes aw@(_ : w) = aw : suffixes w


lazyAST :: Eq a => [a] -> [a] -> STree a
lazyAST = lazyTree edgeAST
    where edgeAST ss = (0, ss)


-- TODO STree to Data.Tree, to be able to print


-- lazyPST :: Eq a => [a] -> [a] -> STree a
-- lazyPST = lazyTree edgeAST


-- lazyCST :: Eq a => [a] -> [a] -> STree a
-- lazyCST = lazyTree edgeAST
