module Main where

import           Data.Char

main :: IO ()
main = interact io

io :: String -> String
io = unlines . addPrefixes . map solve . tail . lines

{-|
>>> solve "hi"
"44 444"
>>> solve "yes"
"999337777"
>>> solve "foo  bar"
"333666 6660 022 2777"
>>> solve "hello world"
"4433555 555666096667775553"
-}
solve :: String -> String
solve = connect . map convert

type Digit = Int
type Times = Int

convert :: Char -> (Digit,Times)
convert c | 'a' <= c && c<= 'c' = (2, ord c - ord 'a' + 1)
          | 'd' <= c && c<= 'f' = (3, ord c - ord 'd' + 1)
          | 'g' <= c && c<= 'i' = (4, ord c - ord 'g' + 1)
          | 'j' <= c && c<= 'l' = (5, ord c - ord 'j' + 1)
          | 'm' <= c && c<= 'o' = (6, ord c - ord 'm' + 1)
          | 'p' <= c && c<= 's' = (7, ord c - ord 'p' + 1)
          | 't' <= c && c<= 'v' = (8, ord c - ord 't' + 1)
          | 'w' <= c && c<= 'z' = (9, ord c - ord 'w' + 1)
          | ' ' == c            = (0, 1)

connect :: [(Digit,Times)] -> String
connect [] = ""
connect [x] = internal x
connect (x:y:xs) | fst x == fst y = internal x  ++ " " ++ connect (y:xs)
                 | fst x /= fst y = internal x ++ connect (y:xs)
internal t = (concatMap show . replicate (snd t) . fst) t

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
