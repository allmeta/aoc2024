import Data.List
import Data.List.Split

main :: IO()
main = do
  file <- readFile "demo.txt"
  putStrLn $ part1 file

part1 = 
  show .
  solve
  . transpose
  . map (map read . words)
  . lines

solve [as,bs] = sum $ map abs $ zipWith (-) (sort as) (sort bs)
