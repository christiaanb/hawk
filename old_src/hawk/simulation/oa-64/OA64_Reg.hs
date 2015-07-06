module OA64_Reg where

import Ix

-- note, use this function to generate registers .....
-- like this:   mkregdecl "R" 127
mkregdecl t n = foldr (\x y -> t++show x++" | "++ y) (t ++ show n) [0 .. (n-1)]



data OA64_Reg 
  -- Integer registers 
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 
  | R14 | R15 | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23 | R24 | R25 | R26 
  | R27 | R28 | R29 | R30 | R31 | R32 | R33 | R34 | R35 | R36 | R37 | R38 | R39 
{- 
  The idea is for these to be included. For now they are removed to save
  on heap space
  | R40 | R41 | R42 | R43 | R44 | R45 | R46 | R47 | R48 | R49 | R50 | R51 | R52 
  | R53 | R54 | R55 | R56 | R57 | R58 | R59 | R60 | R61 | R62 | R63 | R64 | R65 
  | R66 | R67 | R68 | R69 | R70 | R71 | R72 | R73 | R74 | R75 | R76 | R77 | R78 
  | R79 | R80 | R81 | R82 | R83 | R84 | R85 | R86 | R87 | R88 | R89 | R90 | R91 
  | R92 | R93 | R94 | R95 | R96 | R97 | R98 | R99 | R100 | R101 | R102 | R103 
  | R104 | R105 | R106 | R107 | R108 | R109 | R110 | R111 | R112 | R113 | R114 
  | R115 | R116 | R117 | R118 | R119 | R120 | R121 | R122 | R123 | R124 | R125  
  | R126 | R127 
-}

  -- FP registers 
  | F0 | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | F13 
  | F14 | F15 | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23 | F24 | F25 | F26 
  | F27 | F28 | F29 | F30 | F31 | F32 | F33 | F34 | F35 | F36 | F37 | F38 | F39 
{-
  Same here
  | F40 | F41 | F42 | F43 | F44 | F45 | F46 | F47 | F48 | F49 | F50 | F51 | F52 
  | F53 | F54 | F55 | F56 | F57 | F58 | F59 | F60 | F61 | F62 | F63 | F64 | F65 
  | F66 | F67 | F68 | F69 | F70 | F71 | F72 | F73 | F74 | F75 | F76 | F77 | F78 
  | F79 | F80 | F81 | F82 | F83 | F84 | F85 | F86 | F87 | F88 | F89 | F90 | F91 
  | F92 | F93 | F94 | F95 | F96 | F97 | F98 | F99 | F100 | F101 | F102 | F103 
  | F104 | F105 | F106 | F107 | F108 | F109 | F110 | F111 | F112 | F113 | F114 
  | F115 | F116 | F117 | F118 | F119 | F120 | F121 | F122 | F123 | F124 | F125  
  | F126 | F127 
-}


  -- Predicate (Boolean) registers 
  | P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 | P10 | P11 | P12 | P13 
  | P14 | P15 | P16 | P17 | P18 | P19 | P20 | P21 | P22 | P23 | P24 | P25 | P26 
  | P27 | P28 | P29 | P30 | P31 | P32 | P33 | P34 | P35 | P36 | P37 | P38 | P39 
  | P40 | P41 | P42 | P43 | P44 | P45 | P46 | P47 | P48 | P49 | P50 | P51 | P52 
{- and same here...
  | P53 | P54 | P55 | P56 | P57 | P58 | P59 | P60 | P61 | P62 | P63 | P64 | P65 
  | P66 | P67 | P68 | P69 | P70 | P71 | P72 | P73 | P74 | P75 | P76 | P77 | P78 
  | P79 | P80 | P81 | P82 | P83 | P84 | P85 | P86 | P87 | P88 | P89 | P90 | P91 
  | P92 | P93 | P94 | P95 | P96 | P97 | P98 | P99 | P100 | P101 | P102 | P103 
  | P104 | P105 | P106 | P107 | P108 | P109 | P110 | P111 | P112 | P113 | P114 
  | P115 | P116 | P117 | P118 | P119 | P120 | P121 | P122 | P123 | P124 | P125  
  | P126 | P127 

IMPORTANT, set the variable below to P127 when this code is uncommented
-}

  | PC | SpecPC | IAR | Dummy
        deriving (Ix,Eq,Ord,Bounded,Show,Read,Enum)


last_oa64_pred_reg = P52


{-
instance Register OA64_Reg where
     readOnly R0  = True
     readOnly F0  = True
     readOnly _   = False
     ispred p = p `elem` [P1 .. P127]
     pc = PC
     specpc = SpecPC
     ispc x = PC == x
     isspecpc x = SpecPC == x
-}



