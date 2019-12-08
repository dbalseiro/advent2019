import Data.Char (digitToInt)
import Data.List

test :: String
test = "0222112222120000"

parse :: String -> [Int]
parse = fmap digitToInt

dimensions :: (Int, Int)
dimensions = (25,6)

divideIn :: Int -> [a] -> [[a]]
divideIn n [] = []
divideIn n a = take n a : divideIn n (drop n a)

layers :: (Int, Int) -> [a] -> [[a]]
layers (w, h) = divideIn (w*h)

main :: IO ()
main =
  parse . head . lines <$> readFile "input.txt"
  >>= return . analyze
  >>= mapM_ putStrLn

analyze :: [Int] -> [String]
analyze = divideIn (fst dimensions)
          . fmap decodePixel
          . concatMap show
          . formImage
          . layers dimensions

decodePixel :: Char -> Char
decodePixel '0' = ' '
decodePixel '1' = 'X'

formImage :: [[Int]] -> [Int]
formImage (x:[]) = x
formImage (x:y:xs) =
  let pixels = fmap formPixel (zip x y)
   in formImage (pixels:xs)
formImage _ = error "Invalid Image"

formPixel :: (Int, Int) -> Int
formPixel (2, x) = x
formPixel (x, _) = x
