
module SuffixTree.Algorithm.LazyTreeCountSort where

import           Data.Function
import qualified Data.List                  as L
import           Data.Text.Lazy             (Text, cons)
import qualified Data.Text.Lazy             as T
import           Prelude                    (init)
import           Protolude                  hiding (Text)
import           Protolude                  (filter)
import qualified Data.Vector as V

import           SuffixTree.Data.Label      (Label (..))
import           SuffixTree.Data.SuffixTree
import           SuffixTree.Util            as Util

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

-- Tails is about 7%
lazyTreeCount :: Text -> STree
lazyTreeCount text =
    lazyTree'
        (fromIntegral $ T.length text)
        (T.tails text)
    where
        lazyTree' i [""]     = Leaf i
        lazyTree' i suffixes =
            Branch (foldr
                        (addEdge i)
                        []
                        (groupByHead $ V.fromList suffixes))
        addEdge i aGroup edges =
            let
                (lcp, rests) = edgeCST (map tail aGroup)
            in
                case aGroup of
                    (x : _) -> makeEdge x lcp rests : edges
                    []      -> edges
            where
                newLabel mark lcp       = Label mark (succ lcp)
                descendTree lcp         = lazyTree' (i - succ lcp)
                makeEdge mark lcp rests = Edge  (newLabel mark lcp)
                                                (descendTree lcp rests)
