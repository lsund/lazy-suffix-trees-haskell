module Main where

import  Protolude            hiding (filter)
import  Data.Text            as T   (unpack)

-- import Analyze
-- import  Draw
import  Search

main :: IO ()
main =
    do
    text <- readFile "data/book/data.xml"
    alpha <- readFile "data/book/alpha.txt"
    let alphaS = unpack alpha
        textS  = unpack text
        pattern = "ABI. L 185, S. 5"
    print $ indices alphaS textS pattern
