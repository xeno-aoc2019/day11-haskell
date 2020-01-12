module Instruction where

data Instruction = Instruction Int Int Int Int

parseInstruction :: Int -> Instruction
opcode :: Instruction -> Int
mode :: Instruction -> Int -> Int

parseInstruction value = Instruction (parseOpcode value) (parseMode value 1) (parseMode value 2) (parseMode value 3)

opcode (Instruction oc m1 m2 m3) = oc

mode (Instruction oc m1 m2 m3) 1 = m1

mode (Instruction oc m1 m2 m3) 2 = m2

mode (Instruction oc m1 m2 m3) 3 = m3

parseOpcode value = value `mod` 100

parseMode value 1 = (value `div` 100) `mod` 10
parseMode value 2 = (value `div` 1000) `mod` 10
parseMode value 3 = (value `div` 10000) `mod` 10



