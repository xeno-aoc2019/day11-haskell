module Cpu
  ( run
  ) where

import           Instruction
import           Memory

run :: Memory -> Memory
getParam :: Memory -> Instruction -> Int -> Int
paramValue :: Memory -> Mode -> Int -> Int -> Int
paramValue vm MODE_REFERENCE rb value = load vm value
paramValue vm MODE_VALUE rb value     = value
paramValue vm MODE_RELATIVE rb value  = load vm $ rb + value

getParam vm inst index =
  let mod = mode inst index
      param = loadRelative vm index
      rb = getRB vm
   in paramValue vm mod rb param

storeValue :: Memory -> Mode -> Int -> Int -> Int -> Memory
storeValue vm MODE_REFERENCE rb address value = store vm address value
storeValue vm MODE_RELATIVE rb address value  = store vm (rb + address) value

storeParam vm inst index value =
  let mod = mode inst index
      param = loadRelative vm index
      rb = getRB vm
   in storeValue vm mod rb param value

isIoWait1 :: Memory -> Operation -> Bool -> Bool
isIoWait1 vm I_IN False        = True
isIoWait1 vm op inputAvailable = False

isIoWait vm =
  let op = opcode $ getInstruction vm
      inp = hasInput vm
   in isIoWait1 vm op inp

jumpIfTrue :: Memory -> Int -> Mode -> Int -> Int -> Int -> Memory
jumpIfTrue vm 0 mod rb addr steps            = setIP vm steps
jumpIfTrue vm _ MODE_REFERENCE rb addr steps = setIP vm addr
jumpIfTrue vm _ MODE_RELATIVE rb addr steps  = setIP vm (rb + addr)
jumpIfTrue vm _ MODE_VALUE rb addr steps     = setIP vm addr

neg :: Int -> Int
neg 0 = 1
neg 1 = 0

execInstruction :: Operation -> Instruction -> Memory -> Memory
execInstruction I_ADD inst vm =
  let par1 = getParam vm inst 1
      par2 = getParam vm inst 2
      vm1 = storeParam vm inst 3 $ par1 + par2
      vm2 = step vm1 4
   in vm2
execInstruction I_MUL inst vm =
  let par1 = getParam vm inst 1
      par2 = getParam vm inst 2
      vm1 = storeParam vm inst 3 $ par1 * par2
      vm2 = step vm1 4
   in vm2
execInstruction I_IN inst vm =
  let (input, vm1) = getInput vm
      vm2 = storeParam vm1 inst 1 input
      vm3 = step vm2 2
   in vm3
execInstruction I_OUT inst vm =
  let param = getParam vm inst 1
      vm1 = addOutput vm param
      vm2 = step vm1 2
   in vm2
execInstruction I_JT inst vm =
  let param1 = getParam vm inst 1
      addr = loadRelative vm 2
      mod = mode inst 2
      rb = getRB vm
   in jumpIfTrue vm param1 mod rb addr 3
execInstruction I_JF inst vm =
  let param1 = getParam vm inst 1
      addr = loadRelative vm 2
      mod = mode inst 2
      rb = getRB vm
   in jumpIfTrue vm (neg param1) mod rb addr 3
execInstruction I_EQ inst vm =
  let param1 = getParam vm inst 1
      param2 = getParam vm inst 2
      vm1 =
        if param1 == param2
          then storeParam vm inst 3 1
          else storeParam vm inst 3 0
      vm2 = step vm1 4
   in vm2
execInstruction I_LT inst vm =
  let param1 = getParam vm inst 1
      param2 = getParam vm inst 2
      vm1 =
        if param1 < param2
          then storeParam vm inst 3 1
          else storeParam vm inst 3 0
      vm2 = step vm1 4
   in vm2
execInstruction I_RBO inst vm =
  let param1 = getParam vm inst 1
      vm1 = setRB vm param1
      vm2 = step vm1 2
   in vm2
execInstruction I_HALT inst vm = halt vm
execInstruction (I_UNKNOWN n) inst vm = halt $ store vm 10000 n

execCurrent vm =
  let inst = getInstruction vm
      op = opcode inst
   in execInstruction op inst vm

run vm =
  if isIoWait vm || isHalted vm
    then vm
    else run $ execCurrent vm
