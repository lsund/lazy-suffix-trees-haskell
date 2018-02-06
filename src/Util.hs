
module Util where

import Prelude                  (String)
import Protolude
import Data.Char                (chr)
import System.Random

randString :: Int -> IO String
randString n = do
    g <- newStdGen
    let cs = take n $ randoms g :: [Int]
    return $ map (\x -> chr . (+65) . abs $ x `mod` 26) cs

tail :: [a] -> [a]
tail []       = []
tail (_ : xs) = xs

headEq :: Eq a => a -> [a] -> Bool
headEq _ []      = False
headEq a (c : _) = c == a
