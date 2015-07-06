module Symbp6Demo where

import IO
import DLX
import Hawk
import Probe
import Trans
import Symbo

-- main = ptxs 10 u

--t :: Signal [Trans DLX_Op (DLX_Cell DLXReg Int)]
-- t :: [[DLX_Trans Int]]
-- t = view (processor q)

-- u :: [[DLX_Trans SInt]]
-- u = view (processor r)

ptxs n ts =
    let s = foldr (\x y -> show x ++ "\n" ++ y) "" (take n ts)
    in
	putStr s

q :: Num a => ((((a,a),[(a,a,Instr DLXReg a)]),((a,a),[(a,a,a)])),a)
q = ((((64,1023),
       [(64,64,ImmIns (ALUImm (Add Signed)) R3 R0 3),
        (65,65,ImmIns (ALUImm (Add Signed)) R4 R0 4),
	(66,66,ImmIns (ALUImm (Add Signed)) R6 R0 1),
	(67,67,ImmIns (ALUImm (Add Signed)) R5 R0 0),
	(68,68,RegReg ALU (S GreaterEqual) R1 R5 R4),
	(69,69,ImmIns BNEZ R0 R1 32),
	(70,70,Nop),
	(71,71,RegReg ALU Input1 F2 R6 R0),
	(72,72,RegReg ALU Input1 F3 R3 R0),
	(73,73,RegReg ALU (Mult Signed) F2 F2 F3),
	(74,74,RegReg ALU Input1 R6 F2 R0),
	(75,75,ImmIns (ALUImm (Add Signed)) R5 R5 1),
	(76,76,Jmp J ((-36))),
	(77,77,Nop),
	(78,78,RegReg ALU (Add Signed) R1 R0 R6),
	(79,79,Jmp J 4),
	(80,80,Nop),
	(81,81,ImmIns (ALUImm (Add Signed)) R7 R1 0),
	(82,82,Jmp J ((-8))),
	(83,1023,Nop)]),
      ((0,16383),[(0,63,0),(1024,16383,0)])),256)

type SInt = Symbo Int
r :: ((((SInt,SInt),[(SInt,SInt,Instr DLXReg SInt)]),((SInt,SInt),[(SInt,SInt,SInt)])),SInt)
r = ((((64,1023),
       [(64,64,ImmIns (ALUImm (Add Signed)) R3 R0 (Var "x")),
        (65,65,ImmIns (ALUImm (Add Signed)) R4 R0 4),
	(66,66,ImmIns (ALUImm (Add Signed)) R6 R0 1),
	(67,67,ImmIns (ALUImm (Add Signed)) R5 R0 0),
	(68,68,RegReg ALU (S GreaterEqual) R1 R5 R4),
	(69,69,ImmIns BNEZ R0 R1 32),
	(70,70,Nop),
	(71,71,RegReg ALU Input1 F2 R6 R0),
	(72,72,RegReg ALU Input1 F3 R3 R0),
	(73,73,RegReg ALU (Mult Signed) F2 F2 F3),
	(74,74,RegReg ALU Input1 R6 F2 R0),
	(75,75,ImmIns (ALUImm (Add Signed)) R5 R5 1),
	(76,76,Jmp J ((-36))),
	(77,77,Nop),
	(78,78,RegReg ALU (Add Signed) R1 R0 R6),
	(79,79,Jmp J 4),
	(80,80,Nop),
	(81,81,ImmIns (ALUImm (Add Signed)) R7 R1 0),
	(82,82,Jmp J ((-8))),
	(83,1023,Nop)]),
      ((0,16383),[(0,63,0),(1024,16383,0)])),256)

instance Init (Trans DLX_Op (DLX_Cell DLXReg (Symbo Int)))
  where def = pcTrans 256

