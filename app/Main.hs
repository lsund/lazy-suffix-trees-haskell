module Main where

import  Protolude
import  Data.Text            as T   (unpack)
-- import Control.DeepSeq

-- import Analyze
-- import  Draw
import  Search
-- import LazyTree.Functional

main :: IO ()
main = do
    text <- readFile "data/book/data.xml"
    alpha <- readFile "data/book/alpha.txt"
    let alphaS  = unpack alpha
        textS   = unpack text
        -- p = "eg"                    -- 81s
        -- p = "Wegen"                 -- 3s
        p = "ABI. L 185, S. 567"   -- 2s
        -- p = "a"
    -- print $ indices alphaS textS p       -- Get all indices of p
    print $ exists alphaS textS p       -- Does p exist?
