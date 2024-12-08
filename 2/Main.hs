import Data.List
import Data.List.Split

main :: IO()
main = do
  file <- getContents
  let input = map (map read . words) . lines $ file

  print $ solve2 input


solve1 = length . filter isSafe

isSafe xs = (all (>0) ys || all (<0) ys) && all ((<4) . abs) ys
  where ys = zipWith (-) xs $ tail xs

solve2 = length . filter isSafe2

isSafe2 xs = any isSafe ys
  where ys = [removeAtIndex i xs | (i, _) <- zip [0..] xs]

-- Helper function to remove element by index
removeAtIndex :: Int -> [Int] -> [Int]
removeAtIndex i xs = a++b
  where (a,_:b) = splitAt i xs
