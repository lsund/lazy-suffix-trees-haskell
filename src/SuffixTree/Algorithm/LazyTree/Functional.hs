
module SuffixTree.Algorithm.LazyTree.Functional where

import           Data.Function
import qualified Data.List                  as L
import           Data.Text.Lazy             (Text, cons)
import qualified Data.Text.Lazy             as T
import           Prelude                    (String, init)
import           Protolude                  hiding (Text)

import           SuffixTree.Data.Label      (Label (..))
import           SuffixTree.Data.SuffixTree
import           SuffixTree.Util            as Util

-------------------------------------------------------------------------------
-- Atomic Suffix Tree


edgeAST :: EdgeFunction
edgeAST xs = (0, xs)


-------------------------------------------------------------------------------
-- Compact Suffix Tree: Extracts the largest common suffix for each branch


edgeCST :: EdgeFunction
edgeCST []                      = (0, [""])
edgeCST ("" : xss)              = edgeCST xss
edgeCST [s]                     = (T.length s, [""])
edgeCST (xs : xss) =
    let
        (h, t) = (T.head xs, T.tail xs)
        (lcp, xs') = edgeCST (t : removeHeads xss)
    in
        if allHeadsEq h xss
            then (succ lcp, xs')
            else  (0, xs : xss)
    where
        allHeadsEq c = all ((==) c . T.head)


-------------------------------------------------------------------------------
-- Functional LazyTree

filterSuffixes :: Char -> [Text] -> [Text]
filterSuffixes c = map Util.tail . Protolude.filter (headEq c)

lazyTree :: EdgeFunction -> Text -> STree
lazyTree edgeFun x = lazyTree' (fromIntegral $ T.length x) (init $ T.tails x)
    where
        lazyTree' i [""]     = Leaf i
        lazyTree' i suffixes = Branch (foldr' (addEdge i suffixes) [] (L.nub $ heads suffixes))
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


lazyTreeCount :: EdgeFunction -> Text -> STree
lazyTreeCount edgeFun x =
    lazyTree'
        (fromIntegral $ T.length x)
        (init $ T.tails x)
    where
        lazyTree' i [""]     = Leaf i
        lazyTree' i suffixes =
            Branch (foldr'
                        (addEdge i)
                        []
                        (countingSort $ splitSuffixes suffixes))
        addEdge :: Int64 -> [(Char, Text)] -> [Edge] -> [Edge]
        addEdge i aSuffixes' edges =
            let
                aSuffixes = joinSuffixes aSuffixes'
                (lcp, rests) = edgeFun (map snd aSuffixes')
            in
                case aSuffixes' of
                    ((x, xs) : _) -> makeEdge xs lcp rests : edges
                    []         -> edges
            where
                a = fst $ L.head aSuffixes'
                newLabel mark lcp       = Label (a `cons` mark) (succ lcp)
                descendTree lcp         = lazyTree' (i - succ lcp)
                makeEdge mark lcp rests = Edge (newLabel mark lcp)
                                                (descendTree lcp rests)
