module Main where

import           Data.Text                                as Text (unpack, lines)
import           Data.Text.IO                             (readFile)
import           Protolude hiding (readFile)
import           Text.Regex

import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Algorithm.Search
import           SuffixTree.Algorithm.Ukkonen.Functional
import           SuffixTree.Draw
import           SuffixTree.Util

text = "abaababa"
-- text = "agcgacgag"

main = do
    let t1 = lazyTree edgeCST text
    let t2 = ukkonen text
    print t1
    print t2
    -- drawPretty t1
    -- drawPretty t2


-- main = do
--     (path : mode : _) <- getArgs
--     content <- readFile path
--     text <- readFile "data/book/data.xml"
--     alpha <- readFile "data/book/alpha.txt"
--     let nos = lines content
--         alphaS  = unpack alpha
--         textS   = unpack text
--         wrapped = map (\xs -> '(' : unpack xs ++ ")") nos

--         tree = lazyTree edgeCST text
--         -- tree = ukkonen textS

--         regexes = map mkRegex wrapped


--     if mode == "reg" then
--         print $ map (`matchRegex` textS) regexes -- 274 s
--     else
--         print $ map (`exists` tree) nos -- 74s
