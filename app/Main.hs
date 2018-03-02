module Main where

import           Data.List
import           Data.Text                     as T (unpack)
import           Draw
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

main = do
    text <- readFile "data/book/data.xml"
    alpha <- readFile "data/book/alpha.txt"
    let alphaS  = unpack alpha
        textS   = unpack text
        p = "Blauer Stein"                 -- 3s
        -- p = "ABI. L 185, S. 5"         -- 2s

        tree = lazyCST alphaS textS
        -- tree = ukkonen textS

        -- reg = mkRegex "(Blauer Stein)"
        -- reg2 = mkRegex "(Behncke)"

    -- print $ matchRegex reg textS
    -- print $ matchRegex reg2 textS

    -- print $ exists p tree       -- Does p exist?
    print $ indices p tree
