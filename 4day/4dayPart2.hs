import Data.List
import Data.List.Split

-- Reformat so we only have my numbers & the winning numbers
chopper card = map words $ (splitOn "|" . drop 2 . dropWhile (/= ':')) card

-- Number of winning numbers
inScore card = length $ intersect (head card) (last card)

-- Working backwards from last card build a list that represents only cards
-- won on current card. 
 
-- When all cards have been scored return final result
wonCards [] cardList = cardList
-- Last card. It cannot effect anyone else and so it is always zero.
wonCards scoreList [] = wonCards (init scoreList) [0]
-- Calculate a card's whole cascade of won cards by using values already calculated
wonCards scoreList cardList = wonCards (init scoreList) ([scoreCell] ++ cardList)
        -- Only select cards that exist & equal quantity of win 
  where minValidCards = min (length cardList) (last scoreList)
        -- score = # of wins + the values of the cards this card wins
        scoreCell = minValidCards + (sum $ take minValidCards cardList)

main = do
  ipt <- readFile "input"
  -- Calculates number of winning numbers we had
  let scoreList = map (inScore . chopper) $ lines ipt
  -- Score all card's won cards + the card itself. 
  let res = (length $ lines ipt) + (sum $ wonCards scoreList [])
--  print scoreList
  print res
