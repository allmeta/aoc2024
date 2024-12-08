import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- getContents
  let result = mkGrid input
  print result

mkGrid xs =  [ [(y, x, c) | (x, c) <- zip [0..] row] | (y, row) <- zip [0..] (lines xs) ]
