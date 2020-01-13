module Instruction
  ( Instruction
  , Operation(..)
  , parseInstruction
  , mode
  , opcode
  , Mode(..)
  ) where

data Operation
  = I_ADD
  | I_MUL
  | I_IN
  | I_OUT
  | I_JT
  | I_JF
  | I_LT
  | I_EQ
  | I_RBO
  | I_HALT

data Mode
  = MODE_REFERENCE
  | MODE_VALUE
  | MODE_RELATIVE

data Instruction =
  Instruction Operation Mode Mode Mode

parseInstruction :: Int -> Instruction
opcode :: Instruction -> Operation
mode :: Instruction -> Int -> Mode
parseInstruction value =
  Instruction
    (toOperation $ parseOpcode value)
    (toMode $ parseMode value 1)
    (toMode $ parseMode value 2)
    (toMode $ parseMode value 3)

opcode (Instruction oc m1 m2 m3) = oc

mode (Instruction oc m1 m2 m3) 1 = m1
mode (Instruction oc m1 m2 m3) 2 = m2
mode (Instruction oc m1 m2 m3) 3 = m3

parseOpcode value = value `mod` 100

parseMode value 1 = (value `div` 100) `mod` 10
parseMode value 2 = (value `div` 1000) `mod` 10
parseMode value 3 = (value `div` 10000) `mod` 10

toOperation 1  = I_ADD
toOperation 2  = I_MUL
toOperation 3  = I_IN
toOperation 4  = I_OUT
toOperation 5  = I_JT
toOperation 6  = I_JF
toOperation 7  = I_LT
toOperation 8  = I_EQ
toOperation 9  = I_RBO
toOperation 99 = I_HALT

toMode 0 = MODE_REFERENCE
toMode 1 = MODE_VALUE
toMode 2 = MODE_RELATIVE
