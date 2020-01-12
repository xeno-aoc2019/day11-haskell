module Main where

import           Data.Char
import           Input     (readProgramFromFile)
import           Memory    (fromProgram)

main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  let vm = fromProgram program
  print program
  print vm
