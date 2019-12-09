{-# LANGUAGE FlexibleInstances #-}

module Lib (compute) where

import Data.Array (listArray, (!), (//), Array)
import Control.Monad.State.Lazy

class Monad m => IntCode m where
  getInput :: m Integer
  putOutput :: Integer -> m ()

instance IntCode IO where
  getInput = readLn
  putOutput = print

instance IntCode (StateT [Integer] IO) where
  getInput = do
    (x:xs) <- get
    put xs
    return x
  putOutput = put . pure

type IntCodeT m = StateT Base m

type Memory = (Pointer, Array Integer Integer)

data Instruction
  = Halt
  | Add Param Param Param
  | Mul Param Param Param
  | Input Param
  | Output Param
  | JumpT Param Param
  | JumpF Param Param
  | LessThan Param Param Param
  | Equals Param Param Param

data Param = Immediate Integer | Position Integer | Relative Integer

type Pointer = Integer
type Base = Integer

execute :: IntCode m => Base -> Instruction -> Memory -> m Memory
execute base instruction m@(pointer, mem) =
  case instruction of
    Halt -> return m
    (Add p1 p2 pout) -> save 4 pout (getParam base p1 mem + getParam base p2 mem)
    (Mul p1 p2 pout) -> save 4 pout (getParam base p1 mem * getParam base p2 mem)
    (Input pout) -> getInput >>= save 2 pout
    (Output pout) -> putOutput (getParam base pout mem) >> return (pointer + 2, mem)
    (JumpT p1 p2) ->
      let newPointer = if getParam base p1 mem /= 0 then getParam base p2 mem else pointer+3
       in return (newPointer, mem)
    (JumpF p1 p2) ->
      let newPointer = if getParam base p1 mem == 0 then getParam base p2 mem else pointer+3
       in return (newPointer, mem)
    (LessThan p1 p2 pout) ->
      let value = if getParam base p1 mem < getParam base p2 mem then 1 else 0
       in save 4 pout value
    (Equals p1 p2 pout) ->
      let value = if getParam base p1 mem == getParam base p2 mem then 1 else 0
       in save 4 pout value
  where
    save offset pout value = return $ (pointer + offset, mem // [(getParam base pout mem, value)])

readInstruction :: Memory -> Instruction
readInstruction (pointer, mem) =
  let (a,b,c,d,e) = parseOpcode . takeLast 5 . ("00000"++) . show $ mem!pointer
   in case [d,e] of
        "99" -> Halt
        "01" -> Add (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mkParam a (mem!(pointer+3)))
        "02" -> Mul (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mkParam a (mem!(pointer+3)))
        "03" -> Input (mkParam c (mem!(pointer+1)))
        "04" -> Output (mkParam c (mem!(pointer+1)))
        "05" -> JumpT (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2)))
        "06" -> JumpF (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2)))
        "07" -> LessThan (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mkParam a (mem!(pointer+3)))
        "08" -> Equals (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mkParam a (mem!(pointer+3)))
        oc   -> error ("Invalid Opcode: " ++ oc)
  where
    mkParam '0' i = Position i
    mkParam '1' i = Immediate i
    mkParam '2' i = Relative i
    mkParam _ _   = error "Invalid Mode"

    parseOpcode [a,b,c,d,e] = (a,b,c,d,e)
    parseOpcode _           = error ("Invalid opcode length")

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

getParam :: Base -> Param -> Array Integer Integer -> Integer
getParam _ (Immediate i) _ = i
getParam _ (Position pos) mem = mem!pos
getParam base (Relative offset) mem = mem!(base+offset)

compute :: IntCode m => Base -> (Pointer, [Integer]) -> m (Either (Pointer, [Integer]) ())
compute base (p, l) =
  let memory = listArray (0, fromIntegral $ length l - 1) l
   in op base (p, memory)

op :: IntCode m => Base -> Memory -> m (Either (Pointer, [Integer]) ())
op base m =
  let instruction = readInstruction m
   in case instruction of
        Halt -> return (Right ())
        _ -> execute base instruction m >>= op base

