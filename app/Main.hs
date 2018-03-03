module Main where

import           Data.List                     as List hiding (map)
import           Data.Text                     as T (unpack)
import           Prelude                       (String)
import           Protolude
import           Text.Regex

import           SuffixTree.Draw
import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Algorithm.Search
import           SuffixTree.Algorithm.Ukkonen.Functional

text = "abaababa"
-- text = "agcgacgag"

main = do
    let t2 = lazyCST (nub text) text
    drawPretty t2


-- main = do
--     (path : mode : _) <- getArgs
--     content <- readFile path
--     text <- readFile "data/book/data.xml"
--     alpha <- readFile "data/book/alpha.txt"
--     let nos = lines (unpack content)
--         alphaS  = unpack alpha
--         textS   = unpack text
--         wrapped = map (\xs -> '(' : xs ++ ")") nos

--         tree = lazyCST alphaS textS
--         -- tree = ukkonen textS

--         regexes = map mkRegex wrapped


--     if mode == "reg" then
--         print $ map (`matchRegex` textS) regexes -- 274 s
--     else
--         print $ map (`exists` tree) nos -- 74s
