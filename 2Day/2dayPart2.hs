import Data.Maybe
import Data.List
import Data.List.Split

-- We only need the draws 
preSplit game = snd $ (fromJust . uncons) $ splitOneOf ":;" game

-- Split up draws 
chopper game = map (wordsBy (`elem` " ,")) game

-- Create tuple (number, color)
pairOff = map (\pair -> (read $ head pair :: Int, last pair)) 

-- Reformat all draws together
gameGroup game = concat $ map (pairOff . chunksOf 2) game

-- Find maximum for each color & multiply them all together
maxCalc (red, green, blue) [] = red * green * blue
maxCalc (red, green, blue) list
  | color == "red" = maxCalc (max value red, green, blue) (tail list)
  | color == "green" = maxCalc (red, max value green, blue) (tail list)
  | color == "blue" = maxCalc (red, green, max value blue) (tail list)
  where color = snd $ head list
        value = fst $ head list

main = do
  ipt <- readFile "input"
  let res = sum $ map ((maxCalc (0,0,0)) . gameGroup . chopper . preSplit) $ lines ipt
  print res
