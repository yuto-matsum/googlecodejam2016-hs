main :: IO ()
main = interact io

{-|
-}
io :: String -> String
io = unlines . addPrefixes . map solve . parse . tail . lines

parse :: [String] -> [(Int,Int)]
parse [] = []
parse (x:xs) = (read (head x'), read (x' !! 1)) : parse xs where
  x' = words x

possibility :: (Int,Int) -> Bool
possibility (b,m) = m `elem` possibleLengths b

-- INCORRECT
possibleLengths :: Int -> [Int]
possibleLengths 0 = []
possibleLengths 1 = []
possibleLengths 2 = [1]
possibleLengths 3 = [1,2]
possibleLengths x = possibleLengths (x-1) ++ [sum1toY,sum1toY+1] where
  sum1toY = (1+x-2) * (x-2-1+1) `div` 2

{-|
-}
solve :: (Int,Int) -> String
solve x | possibility x = "POSSIBLE\n" ++ proofStr x
        | otherwise     = "IMPOSSIBLE\n"

proofStr :: (Int,Int) -> String
proofStr = unlines . concatMap (map show) . proof

proof :: (Int,Int) -> [[Int]]
proof (b,m) = [line]
  where
    line = map (\x-> if x `elem` ls then 1 else 0) [1..b]
    ls = lengthToDegree m

lengthToDegree :: Int -> [Int]
lengthToDegree x | isDouble (x*8+1) = nums (dsol x)
                 | otherwise        = nums (dsol (x-1)) ++ [dsol (x-1) + 2]

nums x = [2..(x+1)]
isDouble x = ((^2) . truncate . sqrt . fromIntegral) x == x
dsol = (`div` 2) . (\x->x-1) . truncate . sqrt . fromIntegral . (+1) . (*8)

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
