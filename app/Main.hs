module Main where

import           Data.Char
import           Input     (readProgramFromFile)

main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  print program
