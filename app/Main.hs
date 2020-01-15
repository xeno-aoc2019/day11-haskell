module Main where

import           Data.Char
import           Input     (readProgramFromFile)
import           Memory    (fromProgram, store, load, addInput, getInput, addOutput, getOutput)
import           Cpu       (run)

main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  let vm = fromProgram program
  print $ store vm 2 1234
  print $ load vm 1
  let vm1 = addInput vm 1
  let vm2 = addInput vm1 2
  print vm2
  let vm3 = run vm2
  print vm3
