import Data.List
import Data.List.Split

chopper card = map words $ (splitOn "|" . drop 2 . dropWhile (/= ':')) card

inScore card = length $ intersect (head card) (last card)

wonCards [] cardList = cardList
wonCards scoreList [] = wonCards (init scoreList) [0]
wonCards scoreList cardList = wonCards (init scoreList) ([scoreCell] ++ cardList)
  where minValidCards = min (length cardList) (last scoreList)
        scoreCell = minValidCards + (sum $ take minValidCards cardList)

main = do
  ipt <- readFile "input"
  let scoreList = map (inScore . chopper) $ lines ipt
  let res = (length $ lines ipt) + (sum $ wonCards scoreList [])
--  print scoreList
  print res
