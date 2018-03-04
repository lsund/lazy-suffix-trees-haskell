
module SuffixTree.Algorithm.LazyTree.Functional where

import           Data.Function
import           Data.Text                  as Text hiding (all, filter, init,
                                                     map)
import           Prelude                    (init)
import           Protolude                  hiding (length, null)

import           SuffixTree.Data.Label      (Label (..))
import           SuffixTree.Data.SuffixTree
import           SuffixTree.Util

-------------------------------------------------------------------------------
-- Atomic Suffix Tree


edgeAST :: EdgeFunction
edgeAST xs = (0, xs)


-------------------------------------------------------------------------------
-- Compact Suffix Tree: Extracts the largest common suffix for each branch


edgeCST :: EdgeFunction
edgeCST []                      = (0, [])
edgeCST (xs : xss) | null xs    = edgeCST xss
edgeCST [s]                     = (length s, [])
edgeCST suffix@(xs : xss)
  | allHeadsEq (Text.head xs) xss            = (succ lcp, xs')
  | otherwise                   = (0, suffix)
    where
        allHeadsEq c = all ((==) c . Text.head)
        (lcp, xs') = edgeCST (xs : removeHeads xss)


-------------------------------------------------------------------------------
-- Functional LazyTree

-- TODO nub for text before map head suffixes
lazyTree :: EdgeFunction -> Text -> STree
lazyTree edgeFun x = lazyTree' (length x) (init $ Text.tails x)
    where
        lazyTree' i [xs] | null xs     = Leaf i
        lazyTree' i suffixes = Branch (foldr' (addEdge i suffixes) [] (map Text.head suffixes))
        addEdge i suffixes a edges =
            let
                aSuffixes = filterSuffixes a suffixes
                (lcp, rests) = edgeFun aSuffixes
            in
                case aSuffixes of
                    (mark : _) -> makeEdge mark lcp rests : edges
                    []         -> edges
            where
                newLabel mark lcp       = Label (a `cons` mark) (succ lcp)
                descendTree lcp         = lazyTree' (i - succ lcp)
                makeEdge mark lcp rests = Edge (newLabel mark lcp)
                                                (descendTree lcp rests)

filterSuffixes :: Char -> [Text] -> [Text]
filterSuffixes c = map Text.tail . filter (headEq c)

