module Main where

import Lib (compute)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Control.Monad.State

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  print =<< bestPhaseCombo input
  where
    parse = fmap read . splitOn "," . head . lines

bestPhaseCombo :: [Integer] -> IO Integer
bestPhaseCombo input = do
  forM combos $ \phases ->
    (head . snd) <$> runStateT (thrusters phases input) [0]
  >>= return . maximum

combos :: [[Integer]]
combos = [[9,8,7,6,5]]

thrusters :: [Integer] -> [Integer] -> StateT [Integer] IO ()
thrusters phases input =
  forM_ phases $ \phase -> do
    [signal] <- get
    put [phase, signal]
    compute input

test1 :: [Integer]
test1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
