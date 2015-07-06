module OA64_Op where


import Ix
import Hawk
import Word
import Trans
import OA64_Cell
import OA64_Reg



data OA64_Op = ALU AluOp
             | COND AluOp AluOp
             | PAR AluOp AluOp
             | LOAD
             | LOAD_S
             | CHECK_S
             | STORE
             | ERR OA64_Op
             | NOOP
             | FENCE
       deriving (Eq,Show, Read)

isFenceOp FENCE = True
isFenceOp _     = False

instance Instruction OA64_Op where
   isNoOp NOOP = True
   isNoOp _    = False

   isAddOp t = case t of
		ALU (Add _) -> True
              	_ -> False 

   isSubOp t = case t of
             	ALU (Sub _) -> True   
             	_ -> False

   isMultOp t = case t of
             	ALU (Mult _) -> True
            	_ -> False

   isBoolOp t = case t of
             	ALU Xor -> True
             	ALU Not -> True
             	ALU And -> True
             	ALU Or -> True
            	_ -> False
                     
   isDivOp t = case t of
           	ALU (Div _) -> True
            	_ -> False

   isJumpOp t = case t of
             	COND _ _ -> True
             	_ -> False           

   isMemOp t = case t of
                LOAD -> True
                LOAD_S -> True
                STORE  -> True
                _ -> False

 
   isLoadOp t = case t of
		LOAD -> True
		LOAD_S -> True
		CHECK_S -> True
                _ -> False

   isStoreOp t = case t of
		STORE -> True
		_ -> False

   noOp = NOOP 

   isAluOp t = case t of
                ALU _ -> True
                _ -> False

   isCmpOp t = case t of
                ALU (S _) -> True
                _ -> False
 
   aluOp (ALU f) = f

   isCond = isJumpOp

   isPar (PAR _ _) = True
   isPar _ = False

   fstOp (PAR f _) = f
   fstOp (COND f _) = f

   sndOp (PAR f g) = g
   sndOp (COND f g) = g

   memOp op = case op of
                LOAD -> Load FullWord Signed
                LOAD_S -> Load FullWord Signed
                CHECK_S -> NOP
                STORE  -> Store FullWord
                _ -> error "OA64_Op memOp"


isCheckOp CHECK_S = True
isCheckOp _       = False
