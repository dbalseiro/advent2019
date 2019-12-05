import Data.Char (digitToInt)
import Data.List (group, sort)

solution :: Int
solution =
  let range = [171309..643603]
   in length $ filter rules range

rules :: Int -> Bool
rules i =
  let arr = fmap digitToInt . show $ i
   in hasDouble arr && increasing arr

hasDouble :: [Int] -> Bool
hasDouble = elem 2 . fmap length . group

increasing :: [Int] -> Bool
increasing x = x == sort x
