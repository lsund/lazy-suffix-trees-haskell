
module LazyTree.Functional where

import Data.Tree

import Protolude
import Util

-------------------------------------------------------------------------------
-- Public API

lazyAST :: Eq a => [a] -> [a] -> STree a
lazyAST = lazyTree edgeAST

lazyPST :: Eq a => [a] -> [a] -> STree a
lazyPST = lazyTree edgePST

lazyCST :: Eq a => [a] -> [a] -> STree a
lazyCST = lazyTree edgeCST

-------------------------------------------------------------------------------
-- Data

type SuffixList a = [[a]]

type Label a = ([a], Int)

data STree a = Leaf | Branch [(Label a, STree a)] deriving (Eq, Show)

type EdgeFunction a = SuffixList a -> (Int, SuffixList a)


-------------------------------------------------------------------------------
-- Atomic Suffix Tree


edgeAST :: Eq a => EdgeFunction a
edgeAST xs = (0, xs)


-------------------------------------------------------------------------------
-- Position Suffix Tree


-- Takes a list of suffixes and removes the ones that occur in other suffixes
removeNested :: (Eq a) => [[a]] -> [[a]]
removeNested [s]                     = [s]
removeNested suffix@((x : xs) : xss)
    | (not . any (headEq x)) xss     = map tail removed
    | otherwise                      = suffix
        where
            removed                  = removeNested (xs : map tail xss)
removeNested []                      = []
removeNested ([] : _ : _ )           = []

edgePST :: Eq a => EdgeFunction a
edgePST = pstSplit . removeNested
    where
        pstSplit [x] = (length x, [[]])
        pstSplit xs  = (0, xs)


-------------------------------------------------------------------------------
-- Compact Suffix Tree

-- Extracts the the longest common prefix of its suffixes
edgeCST :: Eq a => EdgeFunction a
edgeCST []                      = (-1, [[]])
edgeCST ([] : _ : _ )           = (-1, [[]])
edgeCST [s]                     = (length s, [[]])
edgeCST suffix@((x : xs) : xss)
  | allStartsWith x xss         = (succ cpl, xs')
  | otherwise                   = (0, suffix)
    where
        (cpl, xs')             = edgeCST (xs : map tail xss)
        allStartsWith c = null . filter (not . headEq c)


suffixes :: [a] -> SuffixList a
suffixes = tails


-- select suffixes strating with the character a and di
-- TODO might create a version that does the filter and tail at the same time


toTree :: STree Char -> Tree (Label Char)
toTree t = unfoldTree tuplize $ wrapRoot t
    where tuplize (s, Leaf)      = (s, [])
          tuplize (s, Branch xs) = (s, xs)
          wrapRoot st = (("x", 1 :: Int), st)

lazyTree :: Eq a => EdgeFunction a -> [a] -> [a] -> STree a
lazyTree edge alpha = tree . suffixes
    where
        startsWith a = map tail . filter (headEq a)
        tree [[]] = Leaf
        tree ss   = Branch
                        [((a : sa, succ cpl), tree ssr)
                            | a <- alpha
                            , sa : ssa <- [startsWith a ss]
                            , (cpl, ssr) <- [edge (sa : ssa)]]


