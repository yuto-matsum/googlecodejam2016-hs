module Main where
import           Data.List

main :: IO ()
main = interact io

{-|
-}
io :: String -> String
io = unlines . addPrefixes . map (unwords' . solve) . parse . tail . lines

parse :: [String] -> [(String,String)]
parse = map words'

words' :: String -> (String,String)
words' s = (left,right) where
  left = takeWhile (/=' ') s
  right = (tail . dropWhile (/=' ')) s

unwords' :: (String,String) -> String
unwords' (s,t) = s ++ " " ++ t

{-|
-}
solve :: (String,String) -> (String,String)
solve ([],[])                 = ([],[])
solve (s:ss,t:ts)
  | s=='?' && t=='?'          = offset ('0','0') `tcat` solve (ss,ts)
  | s==t                      = offset (s, t)    `tcat` solve (ss,ts)
  | s/='?' && t/='?' && s'<t' = offset (s, t)    `tcat` (fill9 ss, fill0 ts)
  | s/='?' && t/='?' && s'>t' = offset (s, t)    `tcat` (fill0 ss, fill9 ts)
  | s=='?'                    = offset (t, t)    `tcat` solve (ss,ts)
  | t=='?'                    = offset (s, s)    `tcat` solve (ss,ts) where
    s' = read [s] :: Int
    t' = read [t] :: Int
    offset (c,d) = (show (read [c] + os), show (read [d] + ot))
    os = if nt' - ns' > 5 then 1 else 0
    ot = if ns' - nt' > 5 then 1 else 0
    ns' = if ns/='?' then read [ns] :: Int else 0
    nt' = if nt/='?' then read [nt] :: Int else 0
    ns = if ss/=[] then head ss else '0'
    nt = if ts/=[] then head ts else '0'

tcat (x,y) (z,w) = (x++z, y++w)

fill0 = replace '?' '0'
fill9 = replace '?' '9'

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from to (s:ss) | s==from = to : replace from to ss
                       | s/=from = s  : replace from to ss

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
