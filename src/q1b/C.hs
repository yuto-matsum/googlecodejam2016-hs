import           Data.List

main :: IO ()
main = interact io

{-|
-}
io :: String -> String
io = unlines . addPrefixes . map (show . solve) . parse . tail . lines

type TString = (String,String)
type TInt = (Int,Int)
type Problem = [TString]
type DupInfo = (TString,TInt)

parse :: [String] -> [Problem]
parse [] = []
parse s = parsed : remaining where
  parsed = (map twords . take size . tail) s
  remaining = (parse . drop size . tail) s
  size = (read . head) s

twords :: String -> TString
twords s = ((head . words) s, ((!! 1) . words) s)

{-|
-}
solve :: Problem -> Int
solve ps = if (isNotDup . snd . head) ps' then 0
           else 1 + (solve . map fst . tail) ps' where
  ps' = (sortDup . zipDups) ps

isNotDup :: TInt -> Bool
isNotDup (d1,d2) = d1*d2==0

sortDup :: [DupInfo] -> [DupInfo]
sortDup = sortOn (negate . uncurry (*) . snd)

zipDups :: Problem -> [DupInfo]
zipDups ps = zip ps (tdups ps)

tdups :: [TString] -> [TInt]
tdups = tzip . tdups' . unzip where
  tdups' :: ([String],[String]) -> ([Int],[Int])
  tdups' (xs,ys)= (dups xs, dups ys) where
    dups xs = map (dup xs) xs
    dup xs y = (decrement . length . filter (==y)) xs
    decrement x = x - 1
  tzip (xs,ys) = zip xs ys

addPrefixes :: [String] -> [String]
addPrefixes = zipWith addPrefix [1..] where
  addPrefix :: Int -> String -> String
  addPrefix i s = "Case #" ++ show i ++ ": " ++ s
