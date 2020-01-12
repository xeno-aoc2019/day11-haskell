module Memory
  ( Memory
  , fromProgram
  , Show
  ) where

import           Data.List.Index
import           Data.Map.Strict
import           Prelude         hiding (filter, lookup, map, null)

newtype Memory =
  Memory (Map Int Int)

fromProgram :: [Int] -> Memory
-- store        :: Memory Int Int -> Memory
-- load         :: Memory Int -> Int
instance Show Memory where
  show (Memory m) = show "Memory: " ++ show m

fromProgram program =
  let prog = fromList $ indexed program
   in Memory prog
-- store (Memory vm) position value =
-- load memory position =
