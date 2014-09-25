module Hawk.Arithmetic where

data Sign = Signed | Unsigned
  deriving (Eq,Show)

data Comparison
  = LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq,Show)

data AluOp
  = Add Sign
  | Sub Sign
  | Mult Sign
  | Div Sign
  | And
  | Not
  | Or | Xor
  | Sll | Srl | Sra
  | S Comparison
  | SetHi
  | Input1
  | Input2
  | Invalidate
  deriving (Eq,Show)
