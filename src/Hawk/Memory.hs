module Hawk.Memory where

import Hawk.Arithmetic

data WordSize
  = Byte
  | HalfWord
  | FullWord
  deriving (Eq,Show)

data LoadStoreOp
  = Load WordSize Sign
  | Store WordSize
  | NOP
  deriving (Eq,Show)
