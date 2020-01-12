module Main where

import           Data.Char
import           Input     (readProgramFromFile)
import           Memory    (fromProgram, store, load)

main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  let vm = fromProgram program
  print program
  print vm
  print $ store vm 2 1234
