module Annotations where

import Hawk
import Trans 
import qualified TransSig as T

import DLX

annotate x = lift1 (map anno) x
  where anno t = let Just (Reg PC (Val pc)) = getSrcPC t 
                 in if isBranch t then addInfo (Reg SpecPC (Val pc)) t
                    else t
annotate2 x = lift1 (map anno) x
  where anno t = let Just (Reg PC (Val pc)) = getSrcPC t 
                 in if isBranch t then addInfo (Reg SpecPC (Val $ pc+4)) t
                    else t

annotate3 x = lift1 (map anno) x
  where anno t = if isBranch t then addInfo (Reg SpecPC (Val 264)) t
                    else t

