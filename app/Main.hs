module Main where

import           Data.List
import           Data.Text                     as T (unpack)
import           Draw
import           Protolude

import           Algorithm.LazyTree.Functional
import           Algorithm.Search
import           Algorithm.Ukkonen.Functional

text = "abaababa"
-- text = "agcgacgag"

main = do
    let t = naiveOnline text
    -- print t
    drawPretty t
    -- putStrLn ("-------------------------------\n" :: Text)
    -- let t2 = lazyCST (nub text) text
    -- print t2
    -- drawPretty t2

-- main = do
--     text <- readFile "data/book/data.xml"
--     alpha <- readFile "data/book/alpha.txt"
--     let alphaS  = unpack alpha
--         textS   = unpack text
        -- p = "eg"                    -- 81s
        -- p = "Wegen"                 -- 3s
        -- p = "ABI. L 185, S. 5"         -- 2s
        -- p = "a"
    -- print $ indices alphaS textS p       -- Get all indices of p
    -- print $ exists alphaS textS p       -- Does p exist?
    -- drawFile "data/sample"
