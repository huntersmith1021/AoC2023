import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split

redMax = 12
blueMax = 14 
greenMax = 13

validPull (num, color)
  | color == "red" = (num <= redMax)
  | color == "blue" = (num <= blueMax)
  | otherwise = (num <= greenMax)

preSplit game = (fromJust . uncons) $ splitOneOf ":;" game

chopper (gHead, gTail) = (last $ words gHead, map (wordsBy (`elem` " ,")) gTail)

pairOff = map (\pair -> (read $ head pair :: Int, last pair)) 

gameGroup (gHead, gTail) = (read gHead :: Int, map pairOff $ map (chunksOf 2) gTail)

score game = and $ map validPull $ concat game

play allGames = sum $ map (\(x,y) -> if score y then x else 0) allGames

play2 allgames = map (score . snd) allgames

main = do
  ipt <- readFile "input"
--  let test = head $ lines ipt
--  print $ gameGroup $ chopper $ preSplit test
  let res = play $ map (gameGroup . chopper . preSplit) $ lines ipt
  print res
