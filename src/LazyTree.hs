
module LazyTree where

import Data.Tree

import Protolude
import Util

type Label a = ([a], Int)

data STree a = Leaf | Branch [(Label a, STree a)] deriving (Eq, Show)

type EdgeFunction a = [[a]] -> (Int, [[a]])

-------------------------------------------------------------------------
-- Impl

lazyAST :: Eq a => [a] -> [a] -> STree a
lazyAST = lazyTree edgeAST

lazyPST :: Eq a => [a] -> [a] -> STree a
lazyPST = lazyTree edgePST

lazyCST :: Eq a => [a] -> [a] -> STree a
lazyCST = lazyTree edgeCST

lazyTree :: Eq a => EdgeFunction a -> [a] -> [a] -> STree a
lazyTree edge alpha = tree . suffixes
    where tree [[]] = Leaf
          tree ss   =
            Branch
                [((a : sa, succ cpl), tree ssr) | a <- alpha
                                               , sa : ssa <- [select ss a]
                                               , (cpl, ssr) <- [edge (sa : ssa)]]


-- select suffixes strating with the character a
-- TODO might create a version that does the filter and tail at the same time
select :: Eq a => [[a]] -> a -> [[a]]
select ss a = map tail $ filter (headEq a) ss


-- All non-empty suffixes
-- TODO might need to remove the empty element from here
suffixes :: [a] -> [[a]]
suffixes = tails


-- This function is trivial since the first (only) letter of the edge label has
-- aleready been split off.
edgeAST :: Eq a => EdgeFunction a
edgeAST xs = (0, xs)


-- Similar as AST but takes the whole suffix as an edge label once a suffix
-- list has become unitary. This requires elimination of nested suffixes when
-- they become empty.
edgePST :: Eq a => EdgeFunction a
edgePST = pstLabel . removeNested
    where
        pstLabel [x] = (length x, [[]])
        pstLabel xs  = (0, xs)

-- Takes a list of suffixes and removes the ones that occur in other suffixes
removeNested :: (Eq a) => [[a]] -> [[a]]
removeNested [s]                     = [s]
removeNested suffix@((x : xs) : xss)
    | (not . any (headEq x)) xss     = map tail removed
    | otherwise                      = suffix
        where
            removed                  = removeNested (xs : map tail xss)

-- Extracts the the longest common prefix of its suffixes
edgeCST :: Eq a => EdgeFunction a
edgeCST [s] = (length s, [[]])
edgeCST awss@((a : w) : ss)
  | [] == [0 | c : _ <- ss, a /= c] = (1 + cpl, rss)
  | otherwise                       = (0, awss)
    where (cpl, rss) = edgeCST (w : [u | _ : u <- ss])

-------------------------------------------------------------------------
-- Conversion

toTree :: STree Char -> Tree (Label Char)
toTree t = unfoldTree tuplize $ wrapRoot t
    where tuplize (s, Leaf)      = (s, [])
          tuplize (s, Branch xs) = (s, xs)
          wrapRoot st = (("x", 1 :: Int), st)
