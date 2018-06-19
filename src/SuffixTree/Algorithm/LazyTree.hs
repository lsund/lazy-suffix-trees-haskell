
module SuffixTree.Algorithm.LazyTree where

import           Data.Function
import qualified Data.List                  as L
import           Data.Text.Lazy             (Text, cons)
import qualified Data.Text.Lazy             as T
import           Prelude                    (init, String)
import           Protolude                  hiding (Text)

import           SuffixTree.Data.Label      (Label (..))

type SuffixList = [T.Text]

data Edge = Edge
    { _label   :: Label
    , _subtree :: STree
    } deriving (Eq, Show)

data STree = Leaf | Branch [Edge] deriving (Eq, Show)


heads :: [Text] -> String
heads = foldr (\x acc -> if T.null x then acc else T.head x : acc) []

removeHeads :: [Text] -> [Text]
removeHeads []        = []
removeHeads("" : xss) = removeHeads xss
removeHeads(xs : xss) = T.tail xs : removeHeads xss

filterSuffixes :: Char -> [Text] -> [Text]
filterSuffixes c = map T.tail . filter (\x -> T.head x == c)

dropCommonPrefix :: SuffixList -> (Int64, SuffixList)
dropCommonPrefix []         = (0, [""])
dropCommonPrefix ("" : xss) = dropCommonPrefix xss
dropCommonPrefix [s]        = (T.length s, [""])
dropCommonPrefix (xs : xss) =
    let
        (h, t) = (T.head xs, T.tail xs)
        (lcp, xs') = dropCommonPrefix (t : map T.tail xss)
    in
        if compareHeads h xss
            then (succ lcp, xs')
            else  (0, xs : xss)
    where
        compareHeads c = all ((==) c . T.head)


lazyTree :: Text -> STree
lazyTree x = lazyTree' (init $ T.tails x)
    where
        lazyTree' [""]     = Leaf
        lazyTree' suffixes =
            Branch (foldr' (appendEdge suffixes) [] (L.nub $ heads suffixes))
        appendEdge suffixes a allEdges =
            let
                aSuffixes = filterSuffixes a suffixes
                (lcp, rests) = dropCommonPrefix aSuffixes
            in
                case aSuffixes of
                    (mark : _) -> makeEdge mark lcp rests : allEdges
                    []         -> allEdges
            where
                newLabel mark lcp       = Label (a `cons` mark) (succ lcp)
                descendTree = lazyTree'
                makeEdge mark lcp rests = Edge (newLabel mark lcp)
                                                (descendTree rests)
