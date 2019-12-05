module Lib
    ( compute
    , compute'
    ) where

import Control.Arrow (first, second)
import Data.Array (listArray, (!), (//), Array)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Function ((&))

compute :: [Integer] -> (Integer, Integer) -> Integer
compute l (noun, verb) = op 
  ( (listArray (0, fromIntegral $ length l - 1) l) //
    [(1, noun), (2, verb)]
  )
  0


compute' :: [Integer] -> Integer
compute' l = 
  ((,) <$> [0..99] <*> [0..99]) &
  fmap (\p -> (p, compute l p)) &
  find ((target ==) . snd)      &
  fromJust                      &
  fst                           &
  (\(noun, verb) -> 100 * noun + verb)

target :: Integer
target = 19690720

op :: Array Integer Integer -> Integer -> Integer
op arr pointer
  | arr!pointer == 99 = arr ! 0
  | arr!pointer == 1  = calc (+)
  | arr!pointer == 2  = calc (*)
  | otherwise         = error "Ooops!!"
  where
    calc f   = op 
      (arr // [(arr!(pointer+3), uncurry f operands)]) 
      (pointer+4)
    operands = (arr!(arr!(pointer+1)), arr!(arr!(pointer+2)))

test0 :: [Integer]
test0 = [1,9,10,3,2,3,11,0,99,30,40,50]
test1 :: [Integer]
test1 = [1,0,0,0,99]
test2 :: [Integer]
test2 = [2,3,0,3,99]
test3 :: [Integer]
test3 = [2,4,4,5,99,0]
test4 :: [Integer]
test4 = [1,1,1,4,99,5,6,0,99]
