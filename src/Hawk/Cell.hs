module Hawk.Cell where

import Hawk.Register

class Cell c where
  -- | return a PC register reference with no value
  pcNothing :: Register r => c r w

  -- | return the value within the cell
  getVal :: Register r => c r w -> w

  -- | update the value within the cell
  putVal :: Register r => c r w -> Maybe w -> c r w

  -- | place the cell in an invalid state
  invalidate :: Register r => c r w -> c r w

  -- | is the cell a register reference?
  isReg :: Register r => c r w -> Bool

  -- | is the cell a PC register reference?
  isPC :: Register r => c r w -> Bool
