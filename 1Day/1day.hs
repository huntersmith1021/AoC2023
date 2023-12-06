import Data.List;
import Data.List.Split;

main = do
  ipt <- readFile "input"
  -- Pull out only the numbers from each line of input
  let onlyNums = map (filter (`elem` "0123456789")) $ lines ipt
  -- Sum all lines' first & last chars which represent a two digit number
  let res = sum $ map (\x -> read $ head x:last x:[] :: Int) onlyNums
  print $ res
