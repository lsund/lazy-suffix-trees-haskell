
module Other.LazyTree where

import Protolude
import Data.Tree

-- The type parameter a is the type of the alphabet.
--
-- Edge labels are represented as pairs (s, l) where s is a suffix of the text
-- that contains the edge label as a prefix of length l. Think about other ways
-- of representing the labels?
type Label a = ([a], Int)

-- A suffix tree is eacher a Leaf or a Branch node with a list of
-- (edgelabel, subtree) pairs. These data structures are also used in all later
-- functional algorithms
data STree a = Leaf | Branch [(Label a, STree a)] deriving (Eq, Show)

-- An edgeFunction takes a list of suffixes and splits off a common prefix.
-- Different edge functions are supplied for ast, pst and cst.
type EdgeFunction a = [[a]] -> (Int, [[a]])

-------------------------------------------------------------------------
-- Impl

lazyAST :: Eq a => [a] -> [a] -> STree a
lazyAST = lazyTree edgeAST

lazyPST :: Eq a => [a] -> [a] -> STree a
lazyPST = lazyTree edgePST

lazyCST :: Eq a => [a] -> [a] -> STree a
lazyCST = lazyTree edgeCST

-- The function lazyTree constructs ast, pst or cst depending on the edge
-- function supplied. It takes the list of non-empty suffixes of the text,
-- including the nested suffixes. It groups them by the first letter, applies
-- the edge function and constructs subtrees recursively.
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


-- This function is trivial since the first (only) letter of the edge label has
-- aleready been split off.
edgeAST :: Eq a => EdgeFunction a
edgeAST ss = (0, ss)


-- Similar as AST but takes the whole suffix as an edge label once a suffix
-- list has become unitary. This requires elimination of nested suffixes when
-- they become empty.
edgePST :: Eq a => EdgeFunction a
edgePST = g . elimNested
    where
        g [s] = (length s, [[]])
        g ss  = (0, ss)

elimNested :: (Eq a) => [[a]] -> [[a]]
elimNested [s] = [s]
elimNested awss@((a : w) : ss) | [] == [0 | c : _ <- ss, a /= c] = [a : s | s <- rss]
                               | otherwise                       = awss
                                 where rss = elimNested (w : [u | _ :u <- ss])

-- Extracts the the longest common prefix of its suffixes
edgeCST :: Eq a => EdgeFunction a
edgeCST [s] = (length s, [[]])
edgeCST awss@((a : w) : ss)
  | [] == [0 | c : _ <- ss, a /= c] = (1 + cpl, rss)
  | otherwise                      = (0, awss)
    where (cpl, rss) = edgeCST (w : [u | _ : u <- ss])

-------------------------------------------------------------------------
-- Conversion

toTree :: STree Char -> Tree (Label Char)
toTree t = unfoldTree tuplize $ wrapRoot t
    where tuplize (s, Leaf)      = (s, [])
          tuplize (s, Branch xs) = (s, xs)
          wrapRoot st = (("x", 1 :: Int), st)
