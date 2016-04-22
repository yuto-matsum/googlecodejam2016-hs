module Main where

import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

main :: IO ()
main = interact io

{-|
>>> io "4\n4\n2 3 4 1\n4\n3 3 4 1\n4\n3 3 4 3\n10\n7 8 10 10 9 2 9 6 3 3\n"
"Case #1: 4\nCase #2: 3\nCase #3: 3\nCase #4: 6\n"
-}
io :: String -> String
io = unlines . addPrefixes . map solve . parse . tail . lines

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
>>> parse ["4","2 3 4 1","3","3 2 1"]
[[2,3,4,1],[3,2,1]]
-}
parse :: [String] -> [Problem]
parse [] = []
parse (_:xs) = parsed : parse remaining where
  parsed = (map read . words . head) xs
  remaining = tail xs

{-|
>>> solve [2,3,4,1]
"4"
>>> solve [3,3,4,1]
"3"
>>> solve [3,3,4,3]
"3"
>>> solve [7,8,10,10,9,2,9,6,3,3]
"6"
-}
solve :: Problem -> String
solve p = (show . length) theLongest where
  theLongest :: Circle
  theLongest = List.maximumBy orderByLength anyCircles
  anyCircles = [circle x y | x <- anyPaths, y <- anyPaths]
  anyPaths = pathsOf p

type Circle = [Int]
type Path = [Int]

{-|
>>>circle [1,2,3] [1,2]
[1,2,3]
>>>circle [1,2] [1,2,3]
[1,2,3]
>>>circle [1,2,3,10,11] [6,5,4,3]
[1,2,3,4,5,6]
>>>circle [1,2,3,10,11] [4,3]
[1,2,3,4]
-}
circle :: Path -> Path -> Circle
circle p q | p `isSubpathOf` q = q
           | q `isSubpathOf` p = p
           | otherwise = connectPath p (reverse q)

{-|
>>> [3,4,5] `isSubpathOf` [1,2,3,4,5,6]
True
>>> [3,4,5] `isSubpathOf` [1,2,3,4,5]
True
>>> [3,4,5] `isSubpathOf` [1,2,3,4]
False
>>> [1,2,3] `isSubpathOf` [1,2,3,4,5]
True
-}
isSubpathOf :: Path -> Path -> Bool
isSubpathOf [] _  = True
isSubpathOf _  [] = False
isSubpathOf p q
  | head p == head q = tail p `isSubpathOf` tail q
  | otherwise        = p `isSubpathOf` tail q

{-|
>>>connectPath [1,2,3,10,11] [3,4,5,6]
[1,2,3,4,5,6]
>>>connectPath [1,2,3,10,11] [3,4]
[1,2,3,4]
-}
connectPath :: Path -> Path -> Circle
connectPath [] _  = []
connectPath ps [] = ps
connectPath (p:ps) (q:qs)
  | p == q = q:qs
  | p /= q = p : connectPath ps (q:qs)

{-|
>>> orderByLength [1,2] [3]
GT
>>> orderByLength [1,2] [3,4]
EQ
>>> orderByLength [1,2] [3,4,5]
LT
-}
orderByLength :: Path -> Path -> Ordering
orderByLength p q | lp >  lq = GT
                  | lp <  lq = LT
                  | lp == lq = EQ where
  lp = length p
  lq = length q

{-|
>>> pathsOf [2,3,4,1]
[[1,2,3,4]]
>>> pathsOf [3,3,4,3]
[[1,3,4],[2,3]]
>>> pathsOf [7,8,10,10,9,2,9,6,3,3]
[[1,7,9,3,10],[2,8,6],[4,10],[5,9]]
-}
pathsOf :: Problem -> [Path]
pathsOf p = (trim . walkEachProblems) [1..length p] where
  walkEachProblems = map (walk p)
  trim xs = filter (\x -> rotate x `notElem` xs) xs

rotate :: [a] -> [a]
rotate xs = tail xs ++ [head xs]

{-|
>>> isCube [[1]]
True
>>> isCube [[1,2],[3,4]]
True
>>> isCube [[1,2,3],[4,5,6],[7,8,9]]
True
>>> isCube [[1,2],[3,4],[5,6]]
False
>>> isCube [[1,2,3],[4,5],[7,8,9]]
False
-}
isCube :: [[a]] -> Bool
isCube xs = all (\x -> length x == length xs) xs

{-|
>>> walk [2,3,4,1] 1
[1,2,3,4]
>>> walk [2,3,4,1] 3
[3,4,1,2]
>>> walk [7,8,10,10,9,2,9,6,3,3] 1
[1,7,9,3,10]
-}
walk :: Problem -> Int -> Path
walk p x = (reverse . internal) [x] where
  internal :: [Int] -> Path
  internal (x:xs) | next `notElem` xs = internal (next:x:xs)
                  | otherwise      = x:xs where
    edge = last xs
    next = Maybe.fromJust (Map.lookup x (graph p))

type Arrow = Map.Map Int Int

{-|
>>> graph [2,3,4,1]
fromList [(1,2),(2,3),(3,4),(4,1)]
>>> graph [3,3,4,3]
fromList [(1,3),(2,3),(3,4),(4,3)]
-}
graph :: Problem -> Arrow
graph = Map.fromList . zip [1..]
