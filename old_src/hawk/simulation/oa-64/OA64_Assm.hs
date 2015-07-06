module OA64_Assm where


import Ix
import Hawk
import Word
import Trans
import OA64_Cell
import OA64_Reg
import OA64_RegInst
import OA64_Op




oa_add   r1 r2 r3 p k = Trans [n r1] (ALU (Add Signed))   [n r2,n r3,n p] [c k]
oa_addi  r1 r2 x p k  = Trans [n r1] (ALU (Add Signed))   [n r2,im x,n p] [c k]
oa_uadd  r1 r2 r3 p k = Trans [n r1] (ALU (Add Unsigned)) [n r2,n r3,n p] [c k]
oa_uaddi r1 r2 x p k  = Trans [n r1] (ALU (Add Unsigned)) [n r2,im x,n p] [c k]
oa_sub   r1 r2 r3 p k = Trans [n r1] (ALU (Sub Signed))   [n r2,n r3,n p] [c k]
oa_subi  r1 r2 x p k  = Trans [n r1] (ALU (Sub Signed))   [n r2,im x,n p] [c k]
oa_usub  r1 r2 r3 p k = Trans [n r1] (ALU (Sub Unsigned)) [n r2,n r3,n p] [c k]
oa_usubi r1 r2 x p k  = Trans [n r1] (ALU (Sub Unsigned)) [n r2,im x,n p] [c k]
oa_mult  r1 r2 r3 p k = Trans [n r1] (ALU (Mult Signed))  [n r2,n r3,n p] [c k]
oa_multi r1 r2 x p k  = Trans [n r1] (ALU (Mult Signed))  [n r2,im x,n p] [c k]

oa_umult r r2 r3 p k  = Trans [n r] (ALU (Mult Unsigned)) [n r2,n r3,n p] [c k]
oa_div   r1 r2 r3 p k = Trans [n r1] (ALU (Div Signed))   [n r2,n r3,n p] [c k]
oa_udiv  r1 r2 r3 p k = Trans [n r1] (ALU (Div Unsigned)) [n r2,n r3,n p] [c k]
oa_and   r1 r2 r3 p k = Trans [n r1] (ALU And)            [n r2,n r3,n p] [c k]
oa_or    r1 r2 r3 p k = Trans [n r1] (ALU Or)             [n r2,n r3,n p] [c k]
oa_xor   r1 r2 r3 p k = Trans [n r1] (ALU Xor)            [n r2,n r3,n p] [c k]
oa_not   r1 r2 p k    = Trans [n r1] (ALU Not)            [n r2,n r2,n p] [c k]
oa_sll   r1 r2 r3 p k = Trans [n r1] (ALU Sll)            [n r2,n r3,n p] [c k]
oa_srl   r1 r2 r3 p k = Trans [n r1] (ALU Srl)            [n r2,n r3,n p] [c k]
oa_sra   r1 r2 r3 p k = Trans [n r1] (ALU Sra)            [n r2,n r3,n p] [c k]
oa_lt    r1 r2 r3 p k = Trans [n r1] (ALU (S LessThan))   [n r2,n r3,n p] [c k]
oa_le    r1 r2 r3 p k = Trans [n r1] (ALU (S LessEqual))  [n r2,n r3,n p] [c k]
oa_gt    r r2 r3 p k  = Trans [n r] (ALU (S GreaterThan)) [n r2,n r3,n p] [c k]
oa_ge  r r2 r3 p k  = Trans [n r] (ALU (S GreaterEqual))  [n r2,n r3,n p] [c k]
oa_eq    r1 r2 r3 p k = Trans [n r1] (ALU (S Equal))      [n r2,n r3,n p] [c k]
oa_ne    r1 r2 r3 p k = Trans [n r1] (ALU (S NotEqual))   [n r2,n r3,n p] [c k]
oa_ne2   r1 r1' r2 r3 p k = Trans [n r1,n r1'] 
                                  (ALU (S NotEqual))   
                                  [n r2,n r3,n p] [c k]

oa_load  r1 r2 i p k = Trans [n r1] LOAD  [n r2,Imm i,n p]      [c k]
oa_store r1 i r2 p k = Trans []     STORE [n r1,Imm i,n r2,n p] [c k]

oa_load_s r1 r2 i p k = Trans [n r1] LOAD_S [n r2,Imm i,n p] [c k]
oa_check_s r p k = Trans [n r] CHECK_S [n r,n p] [c k]

oa_beqz r i p k = Trans [n pc] (COND Input1 Input2) 
                        [n r,Imm i,n pc,n p] [c k]
oa_jmpr r1 p k  = Trans [n pc] (ALU Input1)  [n r1,n p]       [c k]
oa_jmp r1 p k = Trans [n pc] (ALU (Add Signed)) [n pc,n r1,n p] [c k]
oa_j    x p k  = Trans [n pc] (ALU (Add Signed)) [im 0,im x,n p] [c k]
--oa_jalr r1 p k  = Trans [n pc,n R127] (PAR Input1 Input2) 
--                        [n r1,n pc,n p] [c k]
--oa_jal  i p k  = Trans [n pc,n R127] (PAR (Add Signed) Input2) 
--                        [i,n pc,n p]    [c k]
oa_trap i  p k  = Trans [n pc,n IAR] (PAR Input1 Input2)  
                        [i,n pc,n p]    [c k]
oa_rfe  i  p k  = Trans [n pc] (ALU Input1)  [n IAR] [c k]

oa_nop = Trans [] NOOP [] [c 0]
oa_fence k = Trans [] FENCE [] [c k]
oa_sync = Trans [] FENCE [] []

n R0 = Reg R0 (Val 0)
n F0 = Reg F0 (Val 0)
n P0 = Reg P0 (Val 0)
n P1 = Reg P1 (Val 1)
n reg = Reg reg Q
im x = Imm x
c k = Comp k


