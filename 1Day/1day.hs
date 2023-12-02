import Data.List;
import Data.List.Split;

main = do
  ipt <- readFile "input"
  let onlyNums = map (filter (`elem` "0123456789")) $ lines ipt
  let res = sum $ map (\x -> read $ head x:last x:[] :: Int) onlyNums
  print $ res
