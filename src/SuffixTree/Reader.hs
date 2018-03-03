module SuffixTree.Reader where

import           Prelude                       (String)

import           Protolude
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec
import           Data.Char
import           Data.Tree


symbol :: String -> Parser (String, Maybe Int)
symbol s = parsecMap (\x -> (x, Nothing)) $ string s <* spaces

-- identifier = do
--     neg <- optionMaybe $ symbol "-"
--     i <- digit
--     return $
--        case neg of
--             Nothing -> i
--             Just _ -> (-i)

-- Parses a word, and then maybe a digit. Then parses the subtree
parseTree :: String -> Parser (Tree (String, Maybe Int))
parseTree x = do
    _ <- symbol "<"
    ci <- parseNum
    _ <- symbol ","
    cj <- parseNum
    _ <- symbol ","
    neg <- optionMaybe $ symbol "-"
    ck <- digit
    _ <- symbol ">"
    subtree <- parseSubTree x
    let i = digitToInt ci
        j = digitToInt cj
        k = if pos then Just (digitToInt ck) else Nothing
        l = take (j - i) $ drop i x
        pos = isNothing neg
    return $ Node (l, k) subtree
    where
        parseNum = do
            _ <- optionMaybe $ symbol "-"
            digit


parseSubTree :: String -> Parser [Tree (String, Maybe Int)]
parseSubTree x = do
    _ <- symbol "["
    trees <- sepBy (parseTree x)  (symbol ",")
    _ <- symbol "]"
    return trees


fileToTree :: FilePath -> IO (Either ParseError (Tree (String, Maybe Int)))
fileToTree = parseFromFile $ parseTree "abaababa"

