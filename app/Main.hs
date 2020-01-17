module Main where

import           Data.Char
import           Input     (readProgramFromFile)
import           Memory    (fromProgram, store, load, addInput, getInput, addOutput, getOutput)
import           Cpu       (run)

main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  let vm = fromProgram program
  let vm1 = run $ addInput vm 1
  let vm2 = run $ addInput vm 2
  let (a1,vm1r) = getOutput vm1
  let (a2,vm2r) = getOutput vm2
  putStrLn ("Answer 1: " ++ show a1)
  putStrLn ("Answer 2: " ++ show a2)
  -- print <- "Answer 2: " ++ show z2