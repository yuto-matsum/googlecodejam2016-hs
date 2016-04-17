import           Test.DocTest

main :: IO ()
main = do
  doctest ["q1a/A.hs"]
  doctest ["q1a/B.hs"]
