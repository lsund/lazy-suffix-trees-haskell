
module Draw where

import Prelude          (String)

import Protolude
import Data.Tree
import Data.Tree.Pretty

import Lib

-------------------------------------------------------------------------
-- Dummy data

tree0 = lazyAST "xa" "xaxa"
tree1 = lazyAST "acg" "agcgacgag"

tree3 = lazyPST "xa" "xaxa"
tree4 = lazyPST "acg" "agcgacgag"

tree5 = lazyCST "xa" "xaxa"
tree6 = lazyCST "acg" "agcgacgag"
-------------------------------------------------------------------------
-- Conversion

toTree :: (Label t, STree t) -> Tree (Label t)
toTree = unfoldTree tuplize
    where tuplize (s, Leaf)      = (s, [])
          tuplize (s, Branch xs) = (s, xs)

-------------------------------------------------------------------------
-- Draw


drawST :: (Tree String -> String) -> STree Char -> IO ()
drawST drawFun = putStr . drawFun . map edgeLabel . toTree . wrapRoot
    where wrapRoot st = (("Root", 1 :: Int), st)
          edgeLabel (s, l) = take l s

draw       = drawST drawTree
drawPretty = drawST drawVerticalTree
