module Hawk.DLX.Reg where

import Control.Lens.Prism
import qualified Hawk.Register as R

data DLXreg
  = R0  | R1  | R2  | R3  | R4  | R5  | R6  | R7
  | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
  | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23
  | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31
  | F0  | F1  | F2  | F3  | F4  | F5  | F6  | F7
  | F8  | F9  | F10 | F11 | F12 | F13 | F14 | F15
  | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23
  | F24 | F25 | F26 | F27 | F28 | F29 | F30 | F31
  | IAR | PC | SpecPC | Dummy
  deriving (Eq,Ord,Show,Enum)

instance R.Register DLXreg where
  -- | Is the register read only.
  readOnly R0    = True
  readOnly Dummy = True
  readOnly _     = False

  reg = prism' fromReg toReg
    where
      fromReg :: R.Reg -> DLXreg
      fromReg R.PC     = PC
      fromReg R.SpecPC = SpecPC
      fromReg R.Other  = Dummy

      toReg PC     = Just R.PC
      toReg SpecPC = Just R.SpecPC
      toReg _      = Just R.Other

