module Cpu (run) where

import Memory
import Instruction

run :: Memory -> Memory

run vm =
  let instr = getInstruction vm
  in vm

