module Main where

import           Data.Char
import           Input     (readProgramFromFile)
import           Memory    (fromProgram, store, load, addInput, getInput, addOutput, getOutput)

main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  let vm = fromProgram program
  print $ store vm 2 1234
  print $ load vm 1
  let vm1 = addInput vm 10
  let vm2 = addInput vm1 20
  print vm2
