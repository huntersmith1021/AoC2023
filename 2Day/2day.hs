import Data.Maybe
import Data.List
import Data.List.Split

-- Note: I realize after seeing question two that this could have been done in 
-- fewer steps but I expected to reuse some for fine grain control in Part 2

redMax = 12
blueMax = 14 
greenMax = 13

validPull (num, color)
  | color == "red" = (num <= redMax)
  | color == "blue" = (num <= blueMax)
  | otherwise = (num <= greenMax)

-- Split into game number & actual game 
preSplit game = (fromJust . uncons) $ splitOneOf ":;" game

-- Reformat input further creating tuple (game number, list of draws) 
chopper (gHead, gTail) = (last $ words gHead, map (wordsBy (`elem` " ,")) gTail)

-- Reformat each draw as a tuple (number drawn, color)
pairOff = map (\pair -> (read $ head pair :: Int, last pair)) 

-- Reformat game number to be an Int & break up draws into a tuple representing them 
gameGroup (gHead, gTail) = (read gHead :: Int, map pairOff $ map (chunksOf 2) gTail)

-- Return if all draws in a game were valid
score game = and $ map validPull $ concat game

-- Use score to calculate which games are valid & sum all valid games
play allGames = sum $ map (\(x,y) -> if score y then x else 0) allGames

main = do
  ipt <- readFile "in1"
  let res = play $ map (gameGroup . chopper . preSplit) $ lines ipt
  print res
