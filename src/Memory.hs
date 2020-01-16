module Memory
  ( Memory
  , fromProgram
  , Show
  , load
  , store
  , addInput
  , getInput
  , hasInput
  , addOutput
  , getOutput
  , isHalted
  , halt
  , getIP
  , setIP
  , getRB
  , setRB
  , step
  , getInstruction
  , loadRelative
  ) where

import           Data.List.Index
import           Data.Map.Strict
import           Data.Sequence
import           Instruction     (Instruction, parseInstruction)
import           Prelude         hiding (filter, lookup, map, null)

data Memory =
  Memory (Map Int Int) (Seq Int) (Seq Int) Int Int Bool ([[Char]])

fromProgram :: [Int] -> Memory
store :: Memory -> Int -> Int -> Memory
load :: Memory -> Int -> Int
addInput :: Memory -> Int -> Memory
addOutput :: Memory -> Int -> Memory
getOutput :: Memory -> (Int, Memory)
getInput :: Memory -> (Int, Memory)
hasInput :: Memory -> Bool
isHalted :: Memory -> Bool
halt :: Memory -> Memory
getIP :: Memory -> Int
setIP :: Memory -> Int -> Memory
getRB :: Memory -> Int
setRB :: Memory -> Int -> Memory
step :: Memory -> Int -> Memory
getInstruction :: Memory -> Instruction
loadRelative :: Memory -> Int -> Int
instance Show Memory where
  show (Memory m input output ip rb halted trace) =
    show "Memory: " ++
    show m ++
    " input=" ++
    show input ++ " output=" ++ show output ++ " IP=" ++ show ip ++ " RB=" ++ show rb ++ " halted=" ++ show halted ++ "trace: " ++ show trace 

fromProgram program =
  let prog = Data.Map.Strict.fromList $ indexed program
   in Memory prog Empty Empty 0 0 False []

hasInput (Memory vm Empty output ip rb halted trace)      = False
hasInput (Memory vm (x :<| xs) output ip rb halted trace) = True

store (Memory vm input output ip rb halted trace) position value =
  Memory (insert position value vm) input output ip rb halted (("store " ++ show position ++ "=" ++ show value) : trace)

load (Memory vm input output ip rb halted trace) position = findWithDefault 0 position vm

getInput (Memory vm (val :<| inp) outp ip rb halted trace) = (val, Memory vm inp outp ip rb halted $ ("get input <- " ++ show val) : trace)

addInput (Memory vm inp outp ip rb halted trace) value = Memory vm (inp |> value) outp ip rb halted $ ("add input -> " ++ show value) : trace

addOutput (Memory vm inp outp ip rb halted trace) value = Memory vm inp (outp |> value) ip rb halted $ ("add output -> " ++ show value) : trace

getOutput (Memory vm inp (val :<| outp) ip rb halted trace) = (val, Memory vm inp outp ip rb halted $ ("get output <- " ++ show val) : trace)

isHalted (Memory vm input output ip rb halted trace) = halted

halt (Memory vm input output ip rb halted trace) = Memory vm input output ip rb True (show "halt" : trace)

getIP (Memory vm input output ip rb halted trace) = ip

setIP (Memory vm input output ip rb halted trace) newIp = Memory vm input output newIp rb halted (("setIP " ++ show newIp) : trace)

step (Memory vm input output ip rb halted trace) steps = Memory vm input output (ip + steps) rb halted (("step "++ show steps) : trace)

getInstruction (Memory vm input output ip rb halted trace) = parseInstruction $ vm ! ip

loadRelative (Memory vm input output ip rb halted trace) n = vm ! (ip + n)

getRB (Memory vm input output ip rb halted trace) = rb

setRB (Memory vm input output ip rb halted trace) newRb = Memory vm input output ip newRb halted (("setRB " ++ show newRb) : trace)
