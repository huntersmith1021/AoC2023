import Data.List
import Data.List.Split

-- Note: I realize that you can trim your range but this doesn't pay

-- Reformat without pointless text & numbers split up
chopper ipt = (form $ head ipt, form $ last ipt)
  where form x = map (\x-> read x :: Int) (words $ drop 11 x)

calcTrav totalTime heldTime = heldTime * (totalTime - heldTime)

timeRange nm = map (calcTrav nm) [0..nm]

score zpr = map (length . \(dist, rang) -> filter (> dist) rang) zpr

main = do
  ipt <- readFile "input"
  let chopped = chopper $ lines ipt
  let time = fst $ chopped
  let dist = snd $ chopped
  let zpr = zip dist (map timeRange time)
  let res = foldr (*) 1 (score zpr)
  print res
