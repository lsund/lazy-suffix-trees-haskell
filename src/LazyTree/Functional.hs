
module LazyTree.Functional where

import Prelude              (String)
import Protolude

import Util

-------------------------------------------------------------------------------
-- Data

type SuffixList a = [[a]]

type Label a = ([a], Int)

type Alphabet a = [a]

type Edge a = (Label a, STree a)

data STree a = Leaf     { _leafNumber :: Int }
             | Branch   { _branches   :: [Edge a] }
             deriving (Eq, Show)

type EdgeFunction a = SuffixList a -> (Int, SuffixList a)


-------------------------------------------------------------------------------
-- Atomic Suffix Tree


edgeAST :: Eq a => EdgeFunction a
edgeAST xs = (0, xs)


-------------------------------------------------------------------------------
-- Position Suffix Tree


-- Takes a list of suffixes and removes the ones that occur in other suffixes
removeNested :: (Eq a) => [[a]] -> [[a]]
removeNested []                      = []
removeNested ([] : _ : _ )           = []
removeNested [s]                     = [s]
removeNested suffix@((x : xs) : xss)
    | (not . any (headEq x)) xss     = map tail removed
    | otherwise                      = suffix
        where
            removed                  = removeNested (xs : map tail xss)


edgePST :: Eq a => EdgeFunction a
edgePST = pstSplit . removeNested
    where
        pstSplit [x] = (length x, [[]])
        pstSplit xs  = (0, xs)


-------------------------------------------------------------------------------
-- Compact Suffix Tree: Extracts the largest common suffix for each branch


edgeCST :: Eq a => EdgeFunction a
edgeCST []                      = (-1, [[]])
edgeCST ([] : _ : _ )           = (-1, [[]])
edgeCST [s]                     = (length s, [[]])
edgeCST suffix@((x : xs) : xss)
  | allStartsWith x xss         = (succ lcp, xs')
  | otherwise                   = (0, suffix)
    where
        (lcp, xs')              = edgeCST (xs : map tail xss)
        allStartsWith c = null . filter (not . headEq c)


-------------------------------------------------------------------------------
-- Functional LazyTree


lazyTree :: Eq a => EdgeFunction a -> Alphabet a -> [a] -> STree a
lazyTree edgeFun as t = lazyTree' (length t) (tails t)
    where
        lazyTree' i [[]]     = Leaf i
        lazyTree' i suffixes = Branch (foldl (addEdge i suffixes) [] as)
        addEdge i suffixes edges a =
            let
                aSuffixes    = groupSuffixes a suffixes
                (lcp, rests) = edgeFun aSuffixes
            in
                case aSuffixes of
                    (mark : _) -> makeEdge mark lcp rests : edges
                    []         -> edges
            where
                groupSuffixes c         = map tail . filter (headEq c)
                newLabel mark lcp       = (a : mark, succ lcp)
                descendTree lcp         = lazyTree' (i - succ lcp)
                makeEdge mark lcp rests = ( newLabel mark lcp
                                           , descendTree lcp rests)

-------------------------------------------------------------------------------
-- Public API

lazyAST :: Eq a => [a] -> [a] -> STree a
lazyAST = lazyTree edgeAST

lazyPST :: Eq a => [a] -> [a] -> STree a
lazyPST = lazyTree edgePST

lazyCST :: Eq a => [a] -> [a] -> STree a
lazyCST = lazyTree edgeCST

unfoldEdge :: Edge Char -> (Label Char, [Edge Char])
unfoldEdge (l, Branch xs)   = (l, xs)
unfoldEdge ((m, n), Leaf i) = (showLeaf, [])
    where
        showLeaf = (m ++ "<" ++ show i ++ ">" :: String, n + 3)

