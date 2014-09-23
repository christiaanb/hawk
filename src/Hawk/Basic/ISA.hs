{-# LANGUAGE DataKinds #-}
module Hawk.Basic.ISA where

import CLaSH.Sized.Signed (Signed)

data RegName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  deriving (Show,Eq,Ord,Enum)

type RegValue = Signed 32

data Opcode = Nop
            | LdImm RegValue
            | Add
            | Sub
            | Mul
            | Load
            | Store
            deriving (Show,Eq)
