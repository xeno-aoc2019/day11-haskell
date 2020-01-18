module Main where

import Data.Char
import Input     (readProgramFromFile)
import Memory    (Memory, fromProgram, store, load, addInput, getInput, addOutput, getOutput, isHalted)
import Cpu       (run)
import Panel

data Action = Action Color Turn

toAction :: Int -> Int -> Action
toAction 0 0 = Action Black LeftTurn
toAction 0 1 = Action Black RightTurn
toAction 1 0 = Action White LeftTurn
toAction 1 1 = Action White RightTurn

colorToInt :: Color -> Int
colorToInt White = 1
colorToInt Black = 0

stepProgram :: Memory -> Int -> (Memory, Action)
stepProgram vm n =
  let vm1 = addInput vm n
      vm2 = run vm1
      (out1,vm3) = getOutput vm2
      (out2,vm4) = getOutput vm3
      action = toAction out1 out2
   in (vm4,action)

performAction :: Outdoors -> Action -> Outdoors
performAction outdoors (Action color turnDir) =
  let outdoors1 = paint outdoors color
      outdoors2 = turnAndMove outdoors1 turnDir
   in outdoors2

executeRobot :: Outdoors -> Memory -> Int -> Outdoors
executeRobot outdoors vm input =
  let (vm1, action) = stepProgram vm input
      outdoors1 = performAction outdoors action
      halted = isHalted vm1
      currentColor = colorAtRobot outdoors1
      nextInput = colorToInt currentColor
   in
      if halted then outdoors1
      else executeRobot outdoors1 vm1 nextInput





main :: IO ()
main = do
  program <- readProgramFromFile "input.txt"
  let vm = fromProgram program
      (vm1, action) = stepProgram vm 0
      outdoors = initialOutdoors
      outdoors2 = executeRobot outdoors vm 0
   in putStrLn ("Painted squares: " ++ show (paintedSquares outdoors2))

--  putStrLn ("Answer 2: " ++ show out2)
