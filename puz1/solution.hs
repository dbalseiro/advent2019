main :: IO ()
main = parseFile <$> readFile "input.txt" >>= print . calculate

parseFile :: String -> [Integer]
parseFile = fmap read . lines

calculate :: [Integer] -> Integer
calculate = sum . fmap mass

mass :: Integer -> Integer
mass m = (m `div` 3) - 2
