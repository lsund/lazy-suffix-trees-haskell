module Main where

import           Data.List                     as List hiding (map)
import           Data.Text                     as T (unpack)
import           Draw
import           Prelude                       (String)
import           Protolude
import           Text.Regex

import           Algorithm.LazyTree.Functional
import           Algorithm.Search
import           Algorithm.Ukkonen.Functional

-- text = "abaababa"
-- text = "agcgacgag"
-- text = "ab"

-- main = do
--     let t2 = lazyCST (nub text) text
--     let t = ukkonen text
--     drawPretty t2
--     drawPretty t

readReferenceNOs :: String -> IO [String]
readReferenceNOs path = do
    content <- readFile path
    return $ lines (unpack content)

main = do
    (path : mode : _) <- getArgs

    nos <- readReferenceNOs path
    text <- readFile "data/book/data.xml"
    alpha <- readFile "data/book/alpha.txt"
    let alphaS  = unpack alpha
        textS   = unpack text
        wrapped = map (\xs -> '(' : xs ++ ")") nos

        tree = lazyCST alphaS textS
        -- tree = ukkonen textS

        -- reg = mkRegex p

        regexes = map mkRegex wrapped

    if mode == "reg" then
        print $ map (`matchRegex` textS) regexes -- 274 s
    else
        print $ map (`exists` tree) nos -- 74s
    -- print $ matchRegex reg textS

    -- print $ exists p tree       -- Does p exist?
    -- print $ indices p tree
