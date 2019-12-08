import Data.Char (digitToInt)
import Data.List

test :: String
test = "123456789012"

parse :: String -> [Int]
parse = fmap digitToInt

dimensions :: (Int, Int)
dimensions = (25, 6)

divideIn :: Int -> [a] -> [[a]]
divideIn n [] = []
divideIn n a = take n a : divideIn n (drop n a)

layers :: (Int, Int) -> [a] -> [[a]]
layers (w, h) = divideIn (w*h)

fewestZeroes :: [[Int]] -> [Int]
fewestZeroes = head . sortOn (count (==0))

checksum :: [Int] -> Int
checksum l = (count (==1) l) * (count (==2) l)

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

main :: IO ()
main =
  parse . head . lines <$> readFile "input.txt"
  >>= print . checksum . fewestZeroes . layers dimensions
