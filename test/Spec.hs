import           Data.List
import           System.Directory
import           Test.DocTest

main :: IO ()
main = do
  ps <- getDirectoryContents "./q1a"
  let files = map ("./q1a/"++) $ filter (isSuffixOf ".hs") ps
      args = map (:[]) files
  mapM_ doctest args
