module Main where

import           Data.List
import           Data.Ord

main :: IO ()
main = interact io

io :: String -> String
io = unlines . addPrefixes . map solve . parse . tail . lines

type Problem = (Int,[Int]) -- (Credit, Prices)

{-|
>>> parse ["100", "3", "5 75 25", "200", "7", "150 24 79 50 88 345 3"]
[(100,[5,75,25]),(200,[150,24,79,50,88,345,3])]
-}
parse :: [String] -> [Problem]
parse [] = []
parse (x:y:z:ws) = (credit, prices) : remaining where
  credit = read x
  prices = (map read . words) z
  remaining = parse ws

{-|
>>> solve (100,[5,75,25])
"2 3"
>>> solve (200,[150,24,79,50,88,345,3])
"1 4"
>>> solve (8,[2,1,9,4,4,56,90,3])
"4 5"
-}
solve :: Problem -> String
solve p = (unwords . map (show . (+1) . fst) . maximumBy comparingPrice . filter (withinCredit c) . iterateChoices . snd) p where
  c = fst p

{-|
>>> iterateChoices [5,75,25]
[[(0,5),(1,75)],[(0,5),(2,25)],[(1,75),(2,25)]]
-}
iterateChoices :: [Int] -> [[(Int,Int)]]
iterateChoices xs = [[kv i, kv j] | i<-[0..n], j<-[(i+1)..n]] where
  kv i = (i, xs !! i)
  n = length xs - 1

withinCredit :: Int -> [(Int,Int)] -> Bool
withinCredit c = (<=c) . sum . map snd

{-|
>>> comparingPrice [(1,5),(2,75)] [(1,5),(3,25)]
GT
-}
comparingPrice :: [(Int,Int)] -> [(Int,Int)] -> Ordering
comparingPrice = comparing (sum . map snd)

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
