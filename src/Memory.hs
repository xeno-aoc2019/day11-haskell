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
  Memory (Map Int Int) (Seq Int) (Seq Int) Int Int Bool

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
  show (Memory m input output ip rb halted) =
    show "Memory: " ++
    show m ++
    " input=" ++
    show input ++ " output=" ++ show output ++ " IP=" ++ show ip ++ " RB=" ++ show rb ++ " halted=" ++ show halted

fromProgram program =
  let prog = Data.Map.Strict.fromList $ indexed program
   in Memory prog Empty Empty 0 0 False

hasInput (Memory vm Empty output ip rb halted)      = False
hasInput (Memory vm (x :<| xs) output ip rb halted) = True

store (Memory vm input output ip rb halted) position value = Memory (insert position value vm) input output ip rb halted

load (Memory vm input output ip rb halted) position = vm ! position

getInput (Memory vm (val :<| inp) outp ip rb halted) = (val, Memory vm inp outp ip rb halted)

addInput (Memory vm inp outp ip rb halted) value = Memory vm (inp |> value) outp ip rb halted

addOutput (Memory vm inp outp ip rb halted) value = Memory vm (inp |> value) outp ip rb halted

getOutput (Memory vm inp (val :<| outp) ip rb halted) = (val, Memory vm inp outp ip rb halted)

isHalted (Memory vm input output ip rb halted) = halted

halt (Memory vm input output ip rb halted) = Memory vm input output ip rb True

getIP (Memory vm input output ip rb halted) = ip

setIP (Memory vm input output ip rb halted) newIp = Memory vm input output newIp rb halted

step (Memory vm input output ip rb halted) steps = Memory vm input output (ip + steps) rb halted

getInstruction (Memory vm input output ip rb halted) = parseInstruction $ vm ! ip

loadRelative (Memory vm input output ip rb halted) n = vm ! (ip + n)

getRB (Memory vm input output ip rb halted) = rb

setRB (Memory vm input output ip rb halted) newRb = Memory vm input output ip newRb halted
