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
  ) where

import           Data.List.Index
import           Data.Map.Strict
import           Data.Sequence
import           Prelude         hiding (filter, lookup, map, null)

data Memory =
  Memory (Map Int Int) (Seq Int) (Seq Int)

fromProgram :: [Int] -> Memory
store :: Memory -> Int -> Int -> Memory
load :: Memory -> Int -> Int
addInput :: Memory -> Int -> Memory
addOutput :: Memory -> Int -> Memory
getOutput :: Memory -> (Int, Memory)
getInput :: Memory -> (Int, Memory)
instance Show Memory where
  show (Memory m input output) = show "Memory: " ++ (show m) ++ " input=" ++ (show input) ++ " output=" ++ (show output)

fromProgram program =
  let prog = Data.Map.Strict.fromList $ indexed program
   in Memory prog Empty Empty

store (Memory vm input output) position value = Memory (insert position value vm) input output

load (Memory vm input output) position = vm ! position

getInput (Memory vm (val :<| inp) outp) = (val, Memory vm inp outp)

addInput (Memory vm inp outp) value = Memory vm (inp |> value) outp

addOutput (Memory vm inp outp) value = Memory vm (inp |> value) outp

getOutput (Memory vm inp (val :<| outp)) = (val, Memory vm inp outp)
