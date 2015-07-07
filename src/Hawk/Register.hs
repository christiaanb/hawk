{-# LANGUAGE ViewPatterns #-}
module Hawk.Register where

import Control.Lens
import Control.Lens.Prism

data Reg
  = PC
  | SpecPC
  | Other
  deriving (Eq,Show)

class Register a where
  -- | Is the register read only.
  readOnly :: a -> Bool
  -- | Pick out the program counter
  reg :: Prism' a Reg

ispc :: Register a => a -> Bool
ispc (preview reg -> (Just PC)) = True
ispc _                          = False

pc :: Register a => a
pc = review reg PC

specpc :: Register a => a
specpc = review reg SpecPC
