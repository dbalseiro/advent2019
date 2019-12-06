module Main where

import Lib (compute)
import Data.List.Split (splitOn)

main :: IO ()
main = parse <$> readFile "input.txt" >>= compute
  where parse = fmap read . splitOn "," . head . lines
