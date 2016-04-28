module Main where

main :: IO ()
main = interact io

io :: String -> String
io = unlines . addPrefixes . map solve . tail . lines

{-|
>>> solve "this is a test"
"test a is this"
>>> solve "foobar"
"foobar"
>>> solve "all your base"
"base your all"
-}
solve :: String -> String
solve = unwords . reverse . words

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
