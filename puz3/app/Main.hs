module Main where

import Lib
import Data.List.Split (splitOn)

tests :: [String]
tests = fmap unlines
  [ [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    , "U62,R66,U55,R34,D71,R55,D58,R83"
    ]
  , [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    ]
  ]

main :: IO ()
main = parse <$> readFile "input.txt" >>= print . intersection

parse :: String -> (Cable, Cable)
parse s =
  let [c1, c2] = fmap (fmap parseStep . splitOn ",") $ lines s
   in (c1, c2)

parseStep :: String -> Step
parseStep (c:n) = Step (parseDirection c) (read n)

parseDirection :: Char -> Direction
parseDirection 'U' = U
parseDirection 'D' = D
parseDirection 'R' = R
parseDirection 'L' = L
