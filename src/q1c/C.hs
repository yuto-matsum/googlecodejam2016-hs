main :: IO ()
main = interact io

{-|
-}
io :: String -> String
io = unlines . addPrefixes . map solve . parse . tail . lines

parse :: [String] -> [a]
parse = undefined

{-|
-}
solve :: a -> String
solve = undefined


addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
