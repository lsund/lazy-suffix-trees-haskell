
module SuffixTree.Util where

import           Data.Char     (chr)
import qualified Data.Text     as T
import           Prelude       (String)
import           Protolude
import           System.Random

randString :: Int -> IO String
randString n = do
    g <- newStdGen
    let cs = take n $ randoms g :: [Int]
    return $ map (\x -> chr . (+65) . abs $ x `mod` 26) cs


tail :: [a] -> [a]
tail []       = []
tail (_ : xs) = xs

removeHeads :: [[a]] -> [[a]]
removeHeads []              = []
removeHeads([] : xss)       = removeHeads xss
removeHeads((_ : xs) : xss) = xs : removeHeads xss

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


headEq :: Eq a => a -> [a] -> Bool
headEq a (c : _) = c == a
headEq _ []      = False


removeDuplicates :: Text -> Text
removeDuplicates ""  =  ""
removeDuplicates t   =  x `T.cons` removeDuplicates withoutX
    where x = T.head t
          withoutX = T.filter (\y -> not (x == y)) (T.tail t)


listify :: (a, a) -> [a]
listify (a, b) = [a, b]


allStartsWith :: Eq a => a -> [[a]] -> Bool
allStartsWith c ((x : _) : xss) = x == c && allStartsWith c xss
allStartsWith _ []              = True
allStartsWith _ [[]]            = True
allStartsWith _ ([] : _)        = True


unconsAll :: [[a]] -> ([a], [[a]])
unconsAll xs =
    let (hs, ts, _) = unconsAll' ([], [], xs)
    in (hs, ts)
unconsAll' (hs, ts, []) = (hs, ts, [])
unconsAll' (hs, ts, (x : xs) : xss) = unconsAll' (x : hs, xs : ts, xss)

longestCommonPrefix :: Eq a => [[a]] -> Int
longestCommonPrefix xs =
    let
        (hs, ts) = unconsAll xs
    in
        if allEq hs then 1 + longestCommonPrefix ts else 0

allEq (x : xs) = all (== x) xs