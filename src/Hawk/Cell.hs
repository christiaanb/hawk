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

  -- | is the value of the cell is known?
  --
  -- @
  -- isVal (r2=6) = True
  -- isVal (r2=?) = False
  -- @
  isAss :: Register r => c r w -> Bool

  -- | is the cell a predicate register reference?
  isPred :: Register r => c r w -> Bool
  isPred _ = False

  -- | true if sameLoc is true and neither cell is invalid
  cellHazard :: (Register r, Eq r, Eq w) => c r w -> c r w -> Bool

  -- | get the register reference
  getReg :: Register r => c r w -> r

  -- | construct a cell with a memory reference
  loc :: w -> c r w
