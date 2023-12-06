import Data.List;

valid = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

convert x
  | x == "one" = "1"
  | x == "two" = "2"
  | x == "three" = "3"
  | x == "four" = "4"
  | x == "five" = "5"
  | x == "six" = "6"
  | x == "seven" = "7"
  | x == "eight" = "8"
  | x == "nine" = "9"
  | otherwise = x


-- Find first valid number by chopping off head until one is found
fstOcc x
  | (length $ fstAv x) > 0 = head $ fstAv x
  | otherwise = fstOcc $ tail x
  where fstAv x = filter (`elem` valid) $ inits x

-- Find last valid number by chopping off the last until one is found
lstOcc x
  | (length $ lstAv x) > 0 = last $ lstAv x
  | otherwise = lstOcc $ init x
  where lstAv x = filter (`elem` valid) $ tails x

justNums x = (convert . fstOcc) x ++ (convert . lstOcc) x

main = do
  ipt <- readFile "input"
  -- Sum all lines' two digit numbers 
  let rst = sum $ map (\x -> read $ justNums x :: Int) $ lines ipt
  print rst
