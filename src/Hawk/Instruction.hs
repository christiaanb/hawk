module Hawk.Instruction where

import Hawk.Arithmetic
import Hawk.Memory

data Instr
  = NoOp
  | Alu AluOp
  | Mem LoadStoreOp

class Instruction i where
  instr :: i -> Instr
  aluOp :: i -> AluOp
  memOp :: i -> LoadStoreOp
  noOp  :: i
