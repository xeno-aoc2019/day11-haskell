module Memory
  ( Memory
  , fromProgram
  , Show
  , load
  , store
  , addInput
  , getInput
  , addOutput
  , getOutput
  , isHalted
  , halt
  , getIP
  , setIP
  , step
  ) where

import           Data.List.Index
import           Data.Map.Strict
import           Data.Sequence
import           Prelude         hiding (filter, lookup, map, null)

data Memory =
  Memory (Map Int Int) (Seq Int) (Seq Int) Int Bool

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
step :: Memory -> Int -> Memory

instance Show Memory where
  show (Memory m input output ip halted) = show "Memory: " ++ show m ++ " input=" ++ show input ++ " output=" ++ show output

fromProgram program =
  let prog = Data.Map.Strict.fromList $ indexed program
   in Memory prog Empty Empty 0 False

hasInput (Memory vm Empty output ip halted) = False
hasInput (Memory vm (x :<| xs) output ip halted) = True

store (Memory vm input output ip halted) position value = Memory (insert position value vm) input output ip halted

load (Memory vm input output ip halted) position = vm ! position

getInput (Memory vm (val :<| inp) outp ip halted) = (val, Memory vm inp outp ip halted)

addInput (Memory vm inp outp ip halted) value = Memory vm (inp |> value) outp ip halted

addOutput (Memory vm inp outp ip halted) value = Memory vm (inp |> value) outp ip halted

getOutput (Memory vm inp (val :<| outp) ip halted) = (val, Memory vm inp outp ip halted)

isHalted (Memory vm input output ip halted) = halted

halt (Memory vm input output ip halted) = Memory vm input output ip True

getIP (Memory vm input output ip halted) = ip

setIP (Memory vm input output ip halted) newIp = Memory vm input output newIp halted

step (Memory vm input output ip halted) steps = Memory vm input output (ip + steps) halted