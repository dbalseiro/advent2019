module Main where

import Lib (compute)
import Data.List.Split (splitOn)

type St = [Integer]

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  compute 0 0 input
  where
    parse = fmap read . splitOn "," . head . lines

