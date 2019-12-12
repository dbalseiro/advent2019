{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib (compute) where

import Data.Array (listArray, (!), (//), Array, Ix)
import Control.Monad.State.Lazy

(!.) :: Ix i => Array i e -> i -> e
(!.) = (!)

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

type Memory = Array Integer Integer

data Registers = Registers { base :: Base, pc :: Pointer, mem :: Memory }

type IntCodeT m = StateT Registers m

increment :: IntCode m => Pointer -> IntCodeT m ()
increment offset = do
  registers@Registers{pc} <- get
  put $ registers { pc = pc + offset }

save :: IntCode m => Pointer -> Param -> Integer -> IntCodeT m ()
save offset pout value = do
  registers@Registers{..} <- get
  idx <- getParam pout
  put $ registers
    { mem = mem // [(idx, value)]
    , pc = pc + offset
    }

execute :: IntCode m => Instruction -> IntCodeT m ()
execute instruction = do
  registers@Registers{..} <- get
  case instruction of
    Halt -> return ()
    (Add p1 p2 pout) -> ((+) <$> getParam p1 <*> getParam p2) >>= save 4 pout
    (Mul p1 p2 pout) -> ((*) <$> getParam p1 <*> getParam p2) >>= save 4 pout
    (Input pout) -> save 2 pout =<< lift getInput
    (Output pout) -> getParam pout >>= lift . putOutput >> increment 2
    (JumpT p1 p2) -> do
      test <- getParam p1
      if test /= 0
         then do
           pc' <- getParam p2
           put $ registers { pc = pc' }
         else increment 3
    (JumpF p1 p2) -> do
      test <- getParam p1
      if test == 0
         then do
           pc' <- getParam p2
           put $ registers { pc = pc' }
         else increment 3
    (LessThan p1 p2 pout) -> do
      test <- (<) <$> getParam p1 <*> getParam p2
      save 4 pout (if test then 1 else 0)
    (Equals p1 p2 pout) -> do
      test <- (==) <$> getParam p1 <*> getParam p2
      save 4 pout (if test then 1 else 0)

readInstruction :: Pointer -> Memory -> Instruction
readInstruction pc mem =
  let (a,b,c,d,e) = parseOpcode . takeLast 5 . ("00000"++) . show $ mem!.pc
   in case [d,e] of
        "99" -> Halt
        "01" -> Add (mkParam c (mem!.(pc+1))) (mkParam b (mem!.(pc+2))) (mkParam a (mem!.(pc+3)))
        "02" -> Mul (mkParam c (mem!.(pc+1))) (mkParam b (mem!.(pc+2))) (mkParam a (mem!.(pc+3)))
        "03" -> Input (mkParam c (mem!.(pc+1)))
        "04" -> Output (mkParam c (mem!.(pc+1)))
        "05" -> JumpT (mkParam c (mem!.(pc+1))) (mkParam b (mem!.(pc+2)))
        "06" -> JumpF (mkParam c (mem!.(pc+1))) (mkParam b (mem!.(pc+2)))
        "07" -> LessThan (mkParam c (mem!.(pc+1))) (mkParam b (mem!.(pc+2))) (mkParam a (mem!.(pc+3)))
        "08" -> Equals (mkParam c (mem!.(pc+1))) (mkParam b (mem!.(pc+2))) (mkParam a (mem!.(pc+3)))
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

getParam :: IntCode m => Param -> IntCodeT m Integer
getParam p = do
  Registers{..} <- get
  return $ case p of
    Immediate i -> i
    Position pos -> mem!.pos
    Relative offset -> mem!.(base+offset)

compute :: IntCode m => Base -> Pointer -> [Integer] -> m ()
compute base pointer l =
  let memory = listArray (0, fromIntegral $ length l - 1) l
   in void $ runStateT op (Registers base pointer memory)

op :: IntCode m => IntCodeT m ()
op = do
  Registers{..} <- get
  let instruction = readInstruction pc mem
  case instruction of
    Halt -> return ()
    _ -> execute instruction >> op

