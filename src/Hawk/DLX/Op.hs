module Hawk.DLX.Op where

import Hawk.Arithmetic
import Hawk.Instruction
import Hawk.Memory

data DLXop
  = ExecOp AluOp
  | CondExecOp AluOp AluOp
  | MemOp LoadStoreOp
  | NoOp

instance Instruction DLXop where
  instr (ExecOp ao) = Alu ao
  instr (MemOp lso) = Mem lso
  instr _           = Hawk.Instruction.NoOp

  aluOp (ExecOp ao) = ao

  memOp (MemOp lso) = lso

  noOp = Hawk.DLX.Op.NoOp
