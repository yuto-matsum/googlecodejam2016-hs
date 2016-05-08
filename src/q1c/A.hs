import           Data.Char  (chr, ord)
import           Data.List  (find)
import           Data.Maybe (fromJust, fromMaybe, isNothing)

main :: IO ()
main = interact io

{-|
-}
io :: String -> String
io = unlines . addPrefixes . map solve . parse . tail . lines

parse :: [String] -> [[Int]]
parse [] = []
parse xs = (map read . words . (!!1)) xs : parse (drop 2 xs)

{-|
-}
solve :: [Int] -> String
solve xs = unwords (solve' xs)

solve' :: [Int] -> [String]
solve' []  = []
solve' xs
  | step /= "" = step : solve' (zipWith (-) xs (fromMaybe (repeat 0) xs'))
  | otherwise  = [] where
    step = toAlpha xs'
    xs' = next xs

toAlpha :: Maybe [Int] -> String
toAlpha Nothing = ""
toAlpha (Just xs) = toAlpha' (ord 'A') xs where
  toAlpha' c [] = ""
  toAlpha' c (2:xs) = chr c: chr c: toAlpha' (c+1) xs
  toAlpha' c (1:xs) = chr c: toAlpha' (c+1) xs
  toAlpha' c (0:xs) = toAlpha' (c+1) xs

next :: [Int] -> Maybe [Int]
next xs = (find (noMajorities xs) . filter (validStep xs) . iterStep . length) xs

validStep :: [Int] -> [Int] -> Bool
validStep xs ys = all (>=0) (zipWith (-) xs ys)

noMajorities :: [Int] -> [Int] -> Bool
noMajorities xs ys = all (==True) (rates (zipWith (-) xs ys))

rates :: [Int] -> [Bool]
rates xs = [x*2 <= sum xs | x<-xs]

iterStep :: Int -> [[Int]]
iterStep 0 = []
iterStep x = concat [iter11,iter2,iter1] where
  iter11 = [plug 1 (plug 1 (replicate (x-2) 0) i) j | i<-[0..(x-2)], j<-[0..(x-1)], i/=j]
  iter2  = [plug 2 (replicate (x-1) 0) i | i<-[0..(x-1)]]
  iter1  = [plug 1 (replicate (x-1) 0) i | i<-[0..(x-1)]]

plug :: a -> [a] -> Int -> [a]
plug x xs n = take n xs ++ [x] ++ drop n xs

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
