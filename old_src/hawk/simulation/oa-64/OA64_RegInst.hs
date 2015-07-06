module OA64_RegInst where

import Ix
import Register
import OA64_Reg

instance Register OA64_Reg where
     readOnly R0  = True
     readOnly F0  = True
     readOnly P0  = True
     readOnly P1  = True
     readOnly _   = False
     ispred p = p `elem` [P0 .. last_oa64_pred_reg]
     pc = PC
     specpc = SpecPC
     ispc x = PC == x
     isspecpc x = SpecPC == x



