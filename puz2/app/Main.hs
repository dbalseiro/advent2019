module Main where

import Lib (compute)

main :: IO ()
main = parse <$> readFile "input.txt" >>= print . compute
  where parse = . head . lines

