module Main where

import qualified Data.Text.Lazy                           as T
import           Data.Text.Lazy.IO                        (readFile)
import           Protolude                                hiding (Text,
                                                           readFile)
import           Text.Regex

import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Algorithm.Search
import           SuffixTree.Algorithm.Ukkonen.Functional
import           SuffixTree.Draw
import           SuffixTree.Util

text = "abaababa"
-- text = "agcgacgag"
-- text = "xabxa$babxba#"
--
sample =  do
    -- let t1 = lazyTree edgeCST text
    let t2 = lazyTreeCount edgeCST text
    let t3 = lazyTreeCountV edgeCST text
    -- let t2 = ukkonen text
    -- drawPretty t1
    drawPretty t2
    drawPretty t3

main = do
    args <- getArgs
    if length args == 1 then
        sample
    else do
        [path, mode] <- getArgs
        content <- readFile path
        text <- readFile "data/book/data.xml"
        alpha <- readFile "data/book/alpha.txt"
        let nos = T.lines content
            alphaS  = T.unpack alpha
            textS   = T.unpack text
            wrapped = map (\xs -> '(' : T.unpack xs ++ ")") nos

            tree = lazyTreeCountV edgeCST text

            regexes = map mkRegex wrapped

        if mode == "reg" then
            print $ map (`matchRegex` textS) regexes -- 274 s
        else
            print $ map (`exists` tree) nos -- 74s
