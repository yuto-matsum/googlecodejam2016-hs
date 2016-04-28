module Main where

main :: IO ()
main = interact io

{-|
>>> io "2\nCAB\nJAM"
"Case #1: CAB\nCase #2: MJA\n"
-}
io :: String -> String
io = unlines . addPrefixes . map solve . tail . lines

{-|
>>> addPrefixes ["A","B","C"]
["Case #1: A","Case #2: B","Case #3: C"]
-}
addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s

{-|
>>> solve "CAB"
"CAB"
>>> solve "JAM"
"MJA"
>>> solve "CODE"
"OCDE"
>>> solve "ABAAB"
"BBAAA"
>>> solve "CABCBBABC"
"CCCABBBAB"
>>> solve "ABCABCABC"
"CCCBAABAB"
>>> solve "ZXCASDQWE"
"ZXCASDQWE"
-}
solve :: String -> String
solve = foldl internal []

{-|
>>> internal "J" 'A'
"JA"
>>> internal "JA" 'M'
"MJA"
-}
internal :: String -> Char -> String
internal [] c = [c]
internal s  c | head s > c = s ++ [c]
              | otherwise  = c:s
