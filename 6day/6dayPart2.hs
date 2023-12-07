import Data.List
import Data.List.Split

-- Note: I realize that you can trim your range but this doesn't pay

-- Reformat both parts as a single number
chopper ipt = (form $ head ipt, form $ last ipt)
  where form x = read (concat $ words $ drop 11 x) :: Int

calcTrav totalTime heldTime = heldTime * (totalTime - heldTime)

timeRange nm = map (calcTrav nm) [0..nm]

main = do
  ipt <- readFile "input"
  let chopped = chopper $ lines ipt
  let time = fst $ chopped
  let dist = snd $ chopped
  let res = (length . filter (> dist) . timeRange) time
  print res 
