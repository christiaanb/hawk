\begin{code}
module DLX
  (
      Instr(..)
     ,BranchFunc(..)
     ,ImmOpcode(..)
     ,RegOpcode(..)
     ,JmpOpcode(..)
     ,DLXReg(..)
     ,SrcReg
     ,DstReg
     ,DLX_Trans
     ,DLX_VTrans
--     ,DLXTrans
     ,DLXCell
--     ,DLX_Word
     ,DLX_Instr
--     ,DLX_InstrMem
     ,dlx2trans
--     ,VDLXTrans
     ,VReg
     ,VTrans
     ,module DLX_Cell
     ,module DLX_Reg
     ,module DLX_Op
  ) where


import Ix
import Hawk
import Word
import Trans
import Prettier
import DLX_Cell
import DLX_Reg
import DLX_Op
import DLX_Pretty
\end{code}

--type DLX_Word = Word32

\begin{code}
-- type DLX_Word = Int
\end{code}

\begin{code}
--type VDLXTrans = VTrans DLXReg DLX_Word
\end{code}

\begin{code}
type VReg a = Virtual a Int
\end{code}

\begin{code}
type VTrans r w = Trans DLX_Op (VReg r)
\end{code}

\begin{code}
instance Init (Trans DLX_Op (DLX_Cell DLXReg Int))
  where def = pcTrans 256
\end{code}

\begin{code}
fillIn x= fillInCells x
\end{code}



\begin{code}
data Instr reg i
           = ImmIns     ImmOpcode reg reg i |
             RegReg     RegOpcode AluOp reg reg reg |
             Jmp        JmpOpcode i |
             Nop
             deriving (Eq,Show, Read)
\end{code}



\begin{code}
data BranchFunc = Never | Always | IfEqZero | IfNeqZero
            deriving (Eq,Show, Read)
\end{code}


\begin{code}
data ImmOpcode = LoadStoreImm LoadStoreOp |
           ALUImm AluOp |
           BEQZ | BNEZ |
           JR |
           JALR
           deriving (Eq,Show, Read)
\end{code}


\begin{code}
data RegOpcode = MOVI2S | MOVS2I |
           ALU
           deriving (Eq,Show, Read)
\end{code}


\begin{code}
data JmpOpcode = J |
           JAL |
           TRAP |
           RFE
           deriving (Eq,Show, Read)
\end{code}




\begin{code}
instance Register DLXReg where
     readOnly R0 = True
     readOnly Dummy = True
     readOnly _  = False
     pc = PC
     specpc = SpecPC
--     specpc = PC
--  bug fix?  Thu Nov 19 18:12:24 PST 1998
     ispc x = PC == x
     isspecpc x = SpecPC == x
\end{code}


\begin{code}
type DLXCell a = DLX_Cell DLXReg a
\end{code}

\begin{code}
type DLXVCell a = DLX_Cell (Virtual DLXReg Int) a
\end{code}

\begin{code}
type DLX_Trans a = Trans DLX_Op (DLXCell a)
\end{code}

\begin{code}
type DLX_VTrans a = Trans DLX_Op (DLXVCell a)
\end{code}

\begin{code}
--type DLXTrans = DLX_Trans DLX_Word
\end{code}

\begin{code}
type DLX_Instr a = Instr DLXReg a
\end{code}

\begin{code}
--type DLX_InstrMem a = InstrMemoryState DLX_Word (DLX_Instr a)
\end{code}

\begin{code}
type SrcReg = DLXReg     -- Source register
\end{code}

\begin{code}
type DstReg = DLXReg     -- Destination register
\end{code}



\begin{code}
regNothing R0 = Reg R0 (Val 0)
regNothing reg = Reg reg NotKnown
\end{code}

\begin{code}
dlx2trans :: Word2 i a => Instr DLXReg i -> DLX_Trans a

dlx2trans (ImmIns (LoadStoreImm loadOp@(Load _ _ )) dest src offset)
  = Trans [regNothing dest] (MemOp loadOp)
          [regNothing src,Imm (toWord offset)] []
 --         [regNothing src,Imm (toWord offset),regNothing Dummy] []

dlx2trans (ImmIns (LoadStoreImm storeOp@(Store _ )) writeAddr writeReg offset)
  = Trans [regNothing Dummy] (MemOp storeOp) [regNothing writeAddr,
                              Imm (toWord offset),
                              regNothing writeReg] []

dlx2trans (ImmIns (ALUImm SetHi) dest _ imm)
  = Trans [destCell] (ExecOp SetHi) [Imm (toWord imm)] []
    where
      destCell = regNothing dest

dlx2trans (ImmIns (ALUImm aluFunc) dest src imm)
  = Trans [destCell] (ExecOp aluFunc) [srcCell,Imm (toWord imm)] []
    where
      destCell = regNothing dest
      srcCell = regNothing src

dlx2trans (ImmIns BEQZ _ src pcOffset)
  = Trans [pcNothing'] (CondExecOp (Add Signed) Input1) [regNothing src,
                                                            pcNothing',
                                                            Imm (toWord pcOffset)]
          []

dlx2trans (ImmIns BNEZ _ src pcOffset)
  = Trans [pcNothing'] (CondExecOp Input1 (Add Signed)) [regNothing src,
                                                            pcNothing',
                                                            Imm (toWord pcOffset)]
    []

dlx2trans (ImmIns JR _ src _ )
  = Trans [pcNothing'] (ExecOp Input1) [regNothing src] []


dlx2trans (RegReg ALU aluFunc dest src1 src2)
  = Trans [regNothing dest] (ExecOp aluFunc) [regNothing src1, regNothing src2] []

dlx2trans (RegReg unknownOp _ _ _ _ )
  = error ("Can't translate " ++ show unknownOp)

dlx2trans (Jmp J offset)
  = Trans [pcNothing'] (ExecOp (Add Signed)) [pcNothing', Imm (toWord offset)] []

dlx2trans (ImmIns JALR _ src _ )
  = Trans [pcNothing',regNothing R31]
          (ParExecOp Input1 Input2)
          [regNothing src, pcNothing'] []

dlx2trans (Jmp JAL offset)
  = Trans [pcNothing',regNothing R31]
          (ParExecOp (Add Signed) Input2)
          [Imm (toWord offset),pcNothing']
          []


dlx2trans (Jmp TRAP offset )
  = Trans [pcNothing',regNothing IAR]
          (ParExecOp Input1 Input2)
          [Imm (toWord offset),pcNothing']
          []


dlx2trans (Jmp RFE _ )
  = Trans [pcNothing'] (ExecOp Input1) [regNothing IAR] []

dlx2trans Nop
--  = Trans [Reg Dummy (Val 0)] (NoOp "dlx2trans") [] []
    = Trans [] (NoOp "dlx2trans") [] []
\end{code}


\begin{code}
pcNothing' = Reg PC NotKnown
\end{code}


\begin{code}
instance Show a => Probe (DLXCell a)
instance Show a => Probe (DLXVCell a)
instance Probe DLXReg
\end{code}

\begin{code}
instance Probe DLX_Op where
  outp (ExecOp (Add _ ))   = "+"
  outp (ExecOp (Sub _ ))   = "-"
  outp (ExecOp (Div _ ))   = "/"
  outp (ExecOp (Mult _ ))  = "*"
  outp (ExecOp op)         = show op
  outp (MemOp (Load _ _))  = "Load"
  outp (MemOp (Store _))   = "Store"
  outp (ParExecOp op1 op2) = "PAR("++outp op1++","++outp op2 ++ ")"
  outp x                   = show x
\end{code}

\begin{code}
instance (Show a, Show b) => Probe (Trans DLX_Op (DLX_Cell a b)) where
    ppout = ppDLX_Trans

{-
instance Show a => Probe (DLX_Trans a) where
    ppout = ppDLX_Trans

instance Show a => Probe (DLX_VTrans a) where
    ppout = ppDLX_Trans
-}
\end{code}

\begin{code}
outInfo [] = ""
outInfo l = "  {" ++ foldr1 (\x y -> x ++ "," ++ y) (map outp l) ++ "}"
\end{code}

\begin{code}
outList [] = ""
outList [x] = outp x
outList l = "[" ++ foldr1 (\x y -> x ++ "," ++ y) (map outp l) ++ "]"
\end{code}
