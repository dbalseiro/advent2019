module Lib (compute) where

import Data.Array (listArray, (!), (//), Array)

type Memory = (Pointer, Array Integer Integer)

data Instruction
  = Halt
  | Add Param Param Out
  | Mul Param Param Out
  | Input Out
  | Output Out
  | JumpT Param Param
  | JumpF Param Param
  | LessThan Param Param Out
  | Equals Param Param Out

data Param = Immediate Integer | Position Integer

type Out = Integer
type Pointer = Integer

execute :: Instruction -> Memory -> IO Memory
execute instruction m@(pointer, mem) =
  case instruction of
    Halt -> return m
    (Add p1 p2 out) -> save 4 out (getParam p1 mem + getParam p2 mem)
    (Mul p1 p2 out) -> save 4 out (getParam p1 mem * getParam p2 mem)
    (Input out) -> putStrLn "? " >> readLn >>= save 2 out
    (Output out) -> print (mem!out) >> return (pointer + 2, mem)
    (JumpT p1 p2) ->
      let newPointer = if getParam p1 mem /= 0 then getParam p2 mem else pointer+3
       in return (newPointer, mem)
    (JumpF p1 p2) ->
      let newPointer = if getParam p1 mem == 0 then getParam p2 mem else pointer+3
       in return (newPointer, mem)
    (LessThan p1 p2 out) ->
      let value = if getParam p1 mem < getParam p2 mem then 1 else 0
       in save 4 out value
    (Equals p1 p2 out) ->
      let value = if getParam p1 mem == getParam p2 mem then 1 else 0
       in save 4 out value
  where
    save offset out value = return $ (pointer + offset, mem // [(out, value)])

readInstruction :: Memory -> Instruction
readInstruction (pointer, mem) =
  let (_,b,c,d,e) = parseOpcode . takeLast 5 . ("00000"++) . show $ mem!pointer
   in case [d,e] of
        "99" -> Halt
        "01" -> Add (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mem!(pointer+3))
        "02" -> Mul (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mem!(pointer+3))
        "03" -> Input (mem!(pointer+1))
        "04" -> Output (mem!(pointer+1))
        "05" -> JumpT (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2)))
        "06" -> JumpF (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2)))
        "07" -> LessThan (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mem!(pointer+3))
        "08" -> Equals (mkParam c (mem!(pointer+1))) (mkParam b (mem!(pointer+2))) (mem!(pointer+3))
        oc   -> error ("Invalid Opcode: " ++ oc)
  where
    mkParam '0' i = Position i
    mkParam '1' i = Immediate i
    mkParam _ _   = error "Invalid Mode"

    parseOpcode [a,b,c,d,e] = (a,b,c,d,e)
    parseOpcode _           = error ("Invalid opcode length")

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

getParam :: Param -> Array Integer Integer -> Integer
getParam (Immediate i) _ = i
getParam (Position pos) mem = mem!pos

compute :: [Integer] -> IO ()
compute l =
  let memory = listArray (0, fromIntegral $ length l - 1) l
   in op (0, memory)

op :: Memory -> IO ()
op m = do
  let instruction = readInstruction m
  case instruction of
    Halt -> return ()
    _    -> execute instruction m >>= op

