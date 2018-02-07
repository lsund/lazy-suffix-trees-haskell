module Reader where

import Prelude      (String)

import Protolude
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import Data.Tree
import Data.Char

-- Reads a tree of the following recursive structure
-- [LABEL[CHILDREN ...LEAF2[]]]
--
-- for example:
-- x[a[b[c1[]]]b[c3[]]c4[]]
-- represents the tree
--
--   x
--   |
-- -----
-- /  |  \
-- a  b  c[4]
-- |  |
-- b  c[3]
-- |
-- c[1]


symbol :: String -> Parser (String, Maybe Int)
symbol s = parsecMap (\x -> (x, Nothing)) $ string s <* spaces


-- Parses a word, and then maybe a digit. Then parses the subtree
parseTree :: Parser (Tree (String, Maybe Int))
parseTree = do
    s <- many1 letter
    mc <- optionMaybe digit
    spaces
    subtree <- parseSubTree
    return $ Node (s, digitToInt <$> mc) subtree


parseSubTree :: Parser [Tree (String, Maybe Int)]
parseSubTree = do
    _ <- symbol "["
    trees <- sepBy parseTree (symbol ",")
    _ <- symbol "]"
    return trees


fileToTree :: FilePath -> IO (Either ParseError (Tree (String, Maybe Int)))
fileToTree = parseFromFile parseTree

