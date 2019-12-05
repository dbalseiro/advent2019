main :: IO ()
main = parseFile <$> readFile "input.txt" >>= print . calculate

parseFile :: String -> [Integer]
parseFile = fmap read . lines

calculate :: [Integer] -> Integer
calculate = sum . fmap totalmass

totalmass :: Integer -> Integer
totalmass = sum . init . mass []

mass :: [Integer] -> Integer -> [Integer]
mass l m =
  let n  = (m `div` 3) - 2
   in if n <= 0 then (m:l) else mass (m:l) n
