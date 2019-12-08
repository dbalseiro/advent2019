module Main where

import Lib (compute)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Control.Monad.State.Lazy

type St = [Integer]
type Input = (Integer, St)
type Result = (St, Either Input ())

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  print =<< bestPhaseCombo (0, input)
  where
    parse = fmap read . splitOn "," . head . lines

bestPhaseCombo :: Input -> IO Integer
bestPhaseCombo input = do
  results <- forM combos $ \(a, b, c, d, e) -> do
    (resultA, Left inputA) <- run [a, 0] input
    (resultB, Left inputB) <- run (b:resultA) input
    (resultC, Left inputC) <- run (c:resultB) input
    (resultD, Left inputD) <- run (d:resultC) input
    (resultE, Left inputE) <- run (e:resultD) input
    feedbackLoop (inputA, inputB, inputC, inputD, inputE) resultE
  return $ maximum results

feedbackLoop :: (Input, Input, Input, Input, Input) -> St -> IO Integer
feedbackLoop (ina, inb, inc, ind, ine) lastResult = do
  a <- run lastResult ina
  b <- run (fst a) inb
  c <- run (fst b) inc
  d <- run (fst c) ind
  e <- run (fst d) ine

  if snd e == Right ()
     then return . head . fst $ e
     else feedbackLoop (getInput a, getInput b, getInput c, getInput d, getInput e) (fst e)

getInput :: Result -> Input
getInput (_, Left input) = input
getInput (_, Right ()) = error "getInput - Invalid Result"

run :: St -> Input -> IO Result
run st input = blargh <$> runStateT (compute input) st

blargh :: (a, b) -> (b, a)
blargh (a, b) = (b, a)

combos :: [(Integer, Integer, Integer, Integer, Integer)]
combos = toTuple <$> permutations [5..9]
  where
    toTuple [a,b,c,d,e] = (a,b,c,d,e)
    toTuple _ = error "Invalid Combo"

test1 :: [Integer]
test1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,
        28,6,99,0,0,5]

test2 :: [Integer]
test2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
