module Main where

import Hawk
import Trans
import qualified PreludeSig as Signaled
import DLX
import Predict
import Processor
import Arithmetic
import Words
import Probe
import Symbo

import IFU(ifu)
import RS(rs)
import ROB(rob)

import Prettier
import DLX_Pretty

import Monad
import LazyST

--main = ptxs 10 u

--t :: Signal [Trans DLX_Op (DLX_Cell DLXReg Int)]
--t :: [[DLX_Trans Int]]
t :: [[Trans DLX_Op (DLX_Cell (Virtual DLXReg Int) Int)]]
t = view (proc q)

--u :: [[DLX_Trans SInt]]
u :: [[Trans DLX_Op (DLX_Cell (Virtual DLXReg Int) (Symbo Int))]]
u = view (proc r)

ptxs n ts =
    let ts' = zip [0..] ts
	q (c, t) d =
	    text (show c ++ ":") <:> indent 5 <:>
	    tab (ppsep ppDLX_Trans line t) <:> line <:> d
	d = foldr q empty (take n ts')
    in
	putStr (pretty 78 d)
	
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

proc ((pgm,pgmdata),startingPC) =  computed'
  where
      (instrs,pc') = ifu (5,pgm) pc ([5,5] `before` space)
      --instrs' = probe "UFO" instrs

      --testTrans = lift1 (\n -> [pcTrans n]) space

      pc   = delay (pcTrans startingPC) npc
      npc = if' miss then' (Signaled.last retired ) 
                     else' pc'

      annotated = delay [] (
                    if' miss then' (lift0 []) 
                             else' (annotate $ filterOut isNop $ instrs)
                    )

      (retired,ready,space,miss) = rob 100 (annotated, computed)
      retired' = probe "R" $! retired
      ready' = probe "Y" ready
      computed' = probe "C" computed
      --miss' = if' miss then' (lift0 $ pcTrans 1) else' (lift0 $ pcTrans 0)
  
      computed = rs (150,execUnits) (delay False miss, delay [] ready)
