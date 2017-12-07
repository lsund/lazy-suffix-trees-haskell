
module Draw where

import Prelude          (String)

import Protolude
import Data.Tree
import Data.Tree.Pretty

import Lib

-------------------------------------------------------------------------
-- Dummy data

dummyST = Branch [(("a", 1), Branch [(("c", 1), Leaf)]), (("b", 1), Leaf)]
dummyLazyAST = lazyAST "xa" "xaxa"

-------------------------------------------------------------------------
-- Conversion

toTree :: (Label t, STree t) -> Tree (Label t)
toTree = unfoldTree tuplize
    where tuplize (l, Leaf)      = (l, [])
          tuplize (l, Branch xs) = (l, xs)

-------------------------------------------------------------------------
-- Draw


drawST :: (Tree String -> String) -> STree Char -> IO ()
drawST drawFun st = putStr $ drawFun $ map fst $ toTree $ wrapRoot st
    where wrapRoot st = (("Root", 1 :: Int), st)

draw       = drawST drawTree
drawPretty = drawST drawVerticalTree
