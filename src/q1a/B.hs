module Main where

import           Data.List (sort)

main :: IO ()
main = interact io

{-|
>>>io "1\n3\n1 2 3\n2 3 5\n3 5 6\n2 3 4\n1 2 3\n"
"Case #1: 3 4 6\n"
-}
io :: String -> String
io = unlines . addPrefixes . map (intsToStr . solve) . parse . tail . lines

{-|
>>>intsToStr [1,2,3,4]
"1 2 3 4"
-}
intsToStr :: [Int] -> String
intsToStr = unwords . map show

{-|
>>> addPrefixes ["A","B","C"]
["Case #1: A","Case #2: B","Case #3: C"]
-}
addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s

type Problem = [Int]

{-|
>>> parse ["2","1 2","2 3","3 4","3","1 2 3","2 3 4","3 4 5","4 5 6","5 6 7"]
[[1,2,2,3,3,4],[1,2,3,2,3,4,3,4,5,4,5,6,5,6,7]]
-}
parse :: [String] -> [Problem]
parse [] = []
parse (s:ss) = problem : parse remaining where
  size = read s :: Int
  lineSize = size * 2 - 1
  problem = (map read . words . unwords . take lineSize) ss
  remaining = drop lineSize ss

{-|
>>>solve [1,2,3,2,3,5,3,5,6,2,3,4,1,2,3]
[3,4,6]
-}
solve :: Problem -> [Int]
solve = map fst . filter (odd . snd) . count

{-|
>>>count ["fizz","buzz","fizz"]
[("buzz",1),("fizz",2)]

>>>count [1,2,2,2,10,3,2,4,4]
[(1,1),(2,4),(3,1),(4,2),(10,1)]
-}
count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count = shrink . map (\x -> (x, 1)) . sort where
  shrink [] = []
  shrink [x] = [x]
  shrink ((x, c):(x', c'):xs)
    | x==x' = shrink ((x, c+c'):xs)
    | x/=x' = (x, c): shrink ((x', c'):xs)
