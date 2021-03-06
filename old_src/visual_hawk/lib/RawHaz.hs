module RawHaz (raw_haz) where

import IO
import DLX
import Hawk
import Probe
import Trans
import Symbo

type SInt = Symbo Int

raw_haz :: ((((SInt,SInt),[(SInt,SInt,Instr DLXReg SInt)]),((SInt,SInt),[(SInt,SInt,SInt)])),SInt)

raw_haz = ((((64,1023),				-- the location of the program
 [(64,64,ImmIns (ALUImm (Add Signed)) R1 R0 (Var "a")),	-- R1 <- a
  (65,65,ImmIns (ALUImm (Add Signed)) R2 R0 (Var "b")),	-- R2 <- b
  (66,66,RegReg ALU (Add Signed) R3 R1 R2),		-- R3 <- R1 + R2
							-- infinite loop
  (67,67,Nop),						-- No_op
  (68,68,ImmIns (ALUImm (Add Signed)) R7 R1 0),		-- R7 <- R1 + 0
  (69,69,Jmp J ((-8))),					-- goto (70-8/4=68)
  (70,1023,Nop)]),					-- No_op in locations 70 through 1023
 ((0,16383),						-- data mem (doesn't matter for
 [(0,63,0),(1024,16383,0)])),				--   this program
							-- memory is 0 through 16383
							-- loc 0-63 have value 0
							-- loc 1024-16383 have value 0
 256)							-- initial pc = 64*4=256


