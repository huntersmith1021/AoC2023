import Data.List
import Data.List.Split

chopper card = map words $ (splitOn "|" . drop 2 . dropWhile (/= ':')) card

score card = winnCalc $ intersect (head card) (last card)
  where winnCalc x
          | length x > 0 = 2 ^ (length x - 1)
          | otherwise = 0

main = do
  ipt <- readFile "input"
  let res = sum $ map (score . chopper) $ lines ipt
  print res
