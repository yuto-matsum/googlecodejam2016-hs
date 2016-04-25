import           System.Directory
import           Test.DocTest

main :: IO ()
main = do
  ps <- getDirectoryContents "./q1a"
  print ps
  -- doctest ps
