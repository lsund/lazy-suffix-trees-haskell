module Main where

import qualified Data.Text.Lazy                           as T
import           Data.Text.Lazy.IO                        (readFile)
import           Protolude                                hiding (Text,
                                                           readFile)
import           Text.Regex

import           SuffixTree.Analyze
import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Algorithm.Search
import           SuffixTree.Algorithm.Ukkonen.Functional
import           SuffixTree.Draw
import           SuffixTree.Util

-- datafile = "data/book/data.xml"
datafile = "data/book/modified.xml"
alphafile = "data/book/alpha.txt"

text = "abaababa"
-- text = "agcgacgag"
-- text = "xabxa$babxba#"
--
sample =  do
    let t1 = lazyTree edgeCST text
    let t3 = lazyTreeCount edgeCST text
    -- let t2 = ukkonen text
    drawPretty t1
    drawPretty t3

main = do
    args <- getArgs
    if length args == 1 then
        sample
    else do
        [path, mode] <- getArgs
        content <- readFile path
        text <- readFile datafile
        -- print $ filter (> 8000) $ map ord $ T.unpack $ alphabet text
        -- print $ filter (> 8000) $ map ord $ T.unpack $ alphabet content
        alpha <- readFile alphafile
        let nos = T.lines content
            alphaS  = T.unpack alpha
            textS   = T.unpack text
            wrapped = map (\xs -> '(' : T.unpack xs ++ ")") nos

            tree = lazyTreeCount edgeCST text

            regexes = map mkRegex wrapped

        if mode == "reg" then
            print $ map (`matchRegex` textS) regexes -- 274 s
        else
            print $ map (`exists` tree) nos -- 74s
