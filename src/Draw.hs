
module Draw where

import Prelude          (String)

import Protolude
import Data.Tree.Pretty
import Data.Tree

import LazyTree
import Reader

-------------------------------------------------------------------------
-- Dummy data

tree0 = lazyAST "xa" "xaxa"
tree1 = lazyAST "acg" "agcgacgag"

tree3 = lazyPST "xa" "xaxa"
tree4 = lazyPST "acg" "agcgacgag"

tree5 = lazyCST "xa" "xaxa"
tree6 = lazyCST "acg" "agcgacgag"

tree7 = lazyAST "abcd" "abc"

-------------------------------------------------------------------------
-- Draw


drawST :: (Tree String -> String) -> STree Char -> IO ()
drawST drawFun = putStr . drawFun . map edgeLabel . toTree
    where
        edgeLabel (s, l) = (take l s)

draw ::STree Char -> IO ()
draw = drawST drawTree

drawPretty :: STree Char -> IO ()
drawPretty = drawST drawVerticalTree

drawFile :: String -> IO ()
drawFile path = do
    res <- fileToTree path
    case res of
      Left e -> putStrLn $ "PARSER ERROR:\n" ++ show e
      Right t -> putStrLn $ drawVerticalTree (map showEdge t)
    where
        showEdge (s, Nothing) = s :: String
        showEdge (s, Just i) = (s :: String) ++ "[" ++ (show i) ++ "]"
