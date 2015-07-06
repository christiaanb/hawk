{-# LANGUAGE DataKinds #-}
module Hawk.DLX where

import Data.Default       (Default)
import CLaSH.Sized.Vector (Vec (..))

import Hawk.Arithmetic
import Hawk.Memory
import Hawk.Trans

import Hawk.DLX.Cell
import Hawk.DLX.Op
import Hawk.DLX.Reg

data Instr reg i
  = ImmIns ImmOpCode reg reg i
  | RegReg RegOpCode AluOp reg reg reg
  | Jmp    JmpOpCode i
  | Nop
  deriving (Eq,Show)

data ImmOpCode
  = LoadStoreImm LoadStoreOp
  | AluImm AluOp
  | BEQZ | BNEZ
  | JR
  | JALR
  deriving (Eq,Show)

data RegOpCode
  = MOVI2S
  | MOVS2I
  | ALU
  deriving (Eq,Show)

data JmpOpCode
  = J
  | JAL
  | TRAP
  | RFE
  deriving (Eq,Show)

type DLX_Cell a  = DLXcell DLXreg a
type DLX_Trans a = Trans 1 2 2 DLXop (DLX_Cell a)
type DLX_Instr a = Instr DLXreg a

regNothing R0  = Reg R0 (Val 0)
regNothing reg = Reg reg NotKnown

dlx2trans :: (Default a, Num a) => DLX_Instr a -> DLX_Trans a
dlx2trans (ImmIns (LoadStoreImm loadOp@(Load _ _)) dest src offset)
  = trans (regNothing dest :> Nil)
          (MemOp loadOp)
          (regNothing src :> Imm offset :> Nil)
          Nil
