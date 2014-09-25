module Hawk.DLX.Cell where

import Data.Default

import Hawk.Cell
import Hawk.Register

data Value a
  = NotKnown
  | Inv
  | Val a
  deriving (Eq,Show)

data DLXcell r w
  = Reg r (Value w)
  | Loc w
  | Imm w

instance (Default w) => Default (DLXcell r w) where
  def = Imm def

instance Cell DLXcell where
  pcNothing = Reg pc NotKnown

  getVal (Reg _ (Val val)) = val
  getVal (Imm val)         = val

  putVal cell          Nothing  = invalidate cell
  putVal reg@(Reg r x) (Just v)
    | readOnly r = reg
    | otherwise  = Reg r (Val v)

  invalidate reg@(Reg r _)
    | readOnly r = reg
    | otherwise  = Reg r Inv
  invalidate imm@(Imm _) = imm

  isReg (Reg _ _) = True
  isReg _         = False

  isPC (Reg x _)  = ispc x
  isPC _          = False

