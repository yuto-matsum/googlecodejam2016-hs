import           Data.List

main :: IO ()
main = interact io

{-|
-}
io :: String -> String
io = unlines . addPrefixes . map solve . tail . lines

{-|
-}
solve :: String -> String
solve = concatMap show . sort . conv

conv :: String -> [Int]
conv "" = []
conv s | exist "EIGHT" s = 8 : (conv . reduce "EIGHT") s
       | exist "ZERO"  s = 0 : (conv . reduce "ZERO") s
       | exist "TWO"   s = 2 : (conv . reduce "TWO") s
       | exist "SIX"   s = 6 : (conv . reduce "SIX") s
       | exist "THREE" s = 3 : (conv . reduce "THREE") s
       | exist "SEVEN" s = 7 : (conv . reduce "SEVEN") s
       | exist "FOUR"  s = 4 : (conv . reduce "FOUR") s
       | exist "FIVE"  s = 5 : (conv . reduce "FIVE") s
       | exist "NINE"  s = 9 : (conv . reduce "NINE") s
       | exist "ONE"   s = 1 : (conv . reduce "ONE") s

exist :: String -> String -> Bool
exist ""     t = True
exist (s:ss) t | s `elem` t = exist ss (delete s t)
               | otherwise  = False

reduce :: String -> String -> String
reduce dels target = foldl (flip delete) target dels

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
