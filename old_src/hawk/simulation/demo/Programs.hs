module Programs where
import Hawk
import DLX


-- projections ....
progmem ((x,y),z) = x
datamem ((x,y),z) = y
ilp = ((((64, 1023), [(64, 64, RegReg ALU (Add Signed) R11 R9 R9), (65, 65, RegReg ALU (Add Signed) R28 R9 R9), (66, 66, RegReg ALU (Sub Signed) R29 R9 R9), (67, 67, RegReg ALU (Add Signed) R30 R9 R9), (68, 68, RegReg ALU (Add Signed) R24 R9 R9), (69, 69, RegReg ALU (Sub Signed) R25 R9 R9), (70, 70, RegReg ALU (Add Signed) R26 R9 R9), (71, 71, RegReg ALU (Sub Signed) R27 R9 R9), (72, 72, RegReg ALU (Add Signed) R28 R9 R9), (73, 73, RegReg ALU (Sub Signed) R29 R9 R9), (74, 74, RegReg ALU (Add Signed) R30 R9 R9), (75, 75, RegReg ALU (Sub Signed) R31 R9 R9), (76, 76, Jmp J ((-52))), (77, 1023, Nop)]), ((0, 16383), [(0, 63, 0), (1024, 16383, 0)])), 256)

no_ilp = ((((64, 1023), [(64, 77, RegReg ALU (Add Signed) R11 R11 R11), (78, 78, Jmp J ((-60))), (79, 1023, Nop)]), ((0, 16383), [(0, 63, 0), (1024, 16383, 0)])), 256)

sum' = ((((64, 1023), [(64, 64, ImmIns (ALUImm (Add Signed)) R1 R0 0), (65, 65, ImmIns (ALUImm (Add Signed)) R2 R0 5), (66, 66, ImmIns BEQZ R0 R2 12), (67, 67, RegReg ALU (Add Signed) R1 R1 R2), (68, 68, ImmIns (ALUImm (Sub Signed)) R2 R2 1), (69, 69, Jmp J ((-16))), (70, 70, ImmIns (ALUImm (Add Signed)) R3 R1 0), (71, 71, Jmp J ((-8))), (72, 1023, Nop)]), ((0, 16383), [(0, 63, 0), (1024, 16383, 0)])), 256)

