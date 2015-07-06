module OA64_Cell where

import Cell
import Register
import Words

type OA64_Compute = Int

data Value a = Q
             | Inv
             | Acid
             | Val a
    deriving (Eq,Show,Read)

data OA64_Cell r w 
          = Reg r (Value w) 
          | Loc w
          | Imm w
          | Comp OA64_Compute
    deriving (Eq,Show,Read)

instance Cell OA64_Cell where
  pcNothing = Reg pc Q
  loc = Loc

  getReg (Reg r _)        = r

  getVal (Reg _ (Val val)) = val
  getVal (Imm val)         = val 
  getVal c           = error ("No data for getData: " ++ show c)

  putVal cell Nothing = invalidate     cell
  putVal reg@(Reg r x) (Just v) 
           | readOnly r = reg
           | otherwise = Reg r (Val v)
                            
  putVal c@(Imm _ ) _   
    = error ("Can't put data into a value cell: " ++ show c)


  invalidate     reg@(Reg r _ ) 
    | readOnly r = reg
    | otherwise  = Reg r Inv
  invalidate     imm@(Imm _ )  = imm





  isReg (Reg _ _ )    = True
  isReg _        = False

  isPred (Reg p _ )  = ispred p
  isPred   _        = False

  isPC (Reg x _)    = ispc x
  isPC _        = False

  isSpecPC (Reg x  _)   = isspecpc x
  isSpecPC _        = False

  isLoc (Loc _)    = True
  isLoc _    = False

  isVal (Imm _ ) = True
  isVal _        = False

  isInv (Reg _ Inv)    = True
  isInv _             = False


  isAss (Reg _ (Val _ )) = True
  isAss (Imm _ )     = True
  isAss _         = False

  isComputed (Reg _ Q) = False
  isComputed _         = True


-- Do the two cells name the same Loc (Reg or PC?)
  sameLoc (Reg reg1 _ ) (Reg reg2 _ ) = reg1 == reg2
  sameLoc _ _        = False


  cellHazard (Reg precReg pRegVal ) (Reg followReg fRegVal )
    | readOnly precReg        = False
    | precReg == followReg    = pRegVal /= Inv && fRegVal /= Inv
                               && pRegVal /= Acid && fRegVal /= Acid
    | True            = False
  cellHazard _ _      = False

isComp (Comp c)  = True
isComp  _        = False

getComp (Comp c) = c
getComp _ = error "getComp"

putComp (Comp c) c' = Comp c'
putComp _ c' = error "putComp"

sizzle     reg@(Reg r _ ) 
    | readOnly r = reg
    | otherwise  = Reg r Acid
sizzle     i  = i







isAcid (Reg _ Acid)    = True
isAcid _             = False
