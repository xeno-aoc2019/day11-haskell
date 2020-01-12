module Memory
  ( Memory
  , fromProgram
  , Show
  , load
  , store
  ) where

import           Data.List.Index
import           Data.Map.Strict
import           Prelude         hiding (filter, lookup, map, null)
import           Data.Sequence

data Memory = Memory (Map Int Int) (Seq Int) (Seq Int)

fromProgram :: [Int] -> Memory
store :: Memory -> Int -> Int -> Memory
load :: Memory -> Int -> Int
instance Show Memory where
  show (Memory m input output) = show "Memory: " ++ show m

fromProgram program =
  let prog = Data.Map.Strict.fromList $ indexed program
   in Memory prog Empty Empty

store (Memory vm input output) position value = Memory (insert position value vm) input output

load (Memory vm input output) position = vm ! position
