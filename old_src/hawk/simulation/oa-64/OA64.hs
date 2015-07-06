module OA64 
( OA64_Word
, OA64_Trans 
, OA64_InstrMem 
, OA64_DataMem 
, module OA64_Cell
, module OA64_Reg
, module OA64_Op
, module OA64_Assm
, assemble
, assembleIO
, getCompute
, hasCompute
, isFence
, isCheck_s
, isLoad_s
, isCheck
) where



import Ix
import Hawk
import Word
import Trans
import OA64_Cell
import OA64_Reg
import OA64_Op
import OA64_Assm
import Probe


type OA64_Trans = Trans OA64_Op (OA64_Cell OA64_Reg OA64_Word)
type OA64_InstrMem = InstrMemoryState OA64_Word OA64_Trans
--type OA64_DataMem = MemoryState OA64_Word OA64_Word
type OA64_DataMem = ((OA64_Word,OA64_Word),[(OA64_Word,OA64_Word,OA64_Word)])

type OA64_Word = Int
--type OA64_Word = Word64
-- WHY ISNT WORD64 a NUM????

---------------------------------------------------------------------
-- EPIC -------------------------------------------------------------

getCompute :: Trans a (OA64_Cell b c) -> OA64_Compute
getCompute (Trans _ _ _ l) = getComp $ head $ filter isComp l

hasCompute :: Trans a (OA64_Cell b c) -> Bool
hasCompute (Trans _ _ _ l) = length (filter isComp l) > 0

isFence :: Trans OA64_Op (c r w) -> Bool
isFence (Trans _ op _ _) = isFenceOp op

isCheck :: Trans OA64_Op (c r w) -> Bool
isCheck (Trans _ op _ _) = isCheckOp op

isLoad_s :: Trans OA64_Op (c r w) -> Bool
isLoad_s (Trans _ LOAD_S _ _) = True
isLoad_s (Trans _ _ _ _) = False

isCheck_s :: Trans OA64_Op (c r w) -> Bool
isCheck_s (Trans _ CHECK_S _ _) = True
isCheck_s (Trans _ _ _ _) = False

---------------------------------------------------------------------
-- Assembler---------------------------------------------------------

assembleIO :: FilePath -> [OA64_Trans] -> IO()
assembleIO f pgm = 
  do { putStrLn $ "writing " ++ f
     ; writeFile f (show $ assemble pgm)
     }

assemble :: [OA64_Trans] -> ((OA64_InstrMem,OA64_DataMem),OA64_Word)
assemble pgm = ((p,d),start)
  where
  p = i_space (start,border) pgm
  d = d_space (border+1,end) 0
  start = 1
  border = length pgm + 200 
  end = 16388


  i_space :: (OA64_Word,OA64_Word) -> [OA64_Trans] -> OA64_InstrMem
  i_space (b,e) pgm = ((b,e),pgm'')
    where
    pgm_len = length pgm
    pgm' = zipWith (\x y -> (x,x,y)) [b .. ] pgm
    pgm'' = if pgm_len > e then err else pgm' ++ [(pgm_len+1,e,oa_nop)]
    err = error $ "assemble: size " ++ show e ++ " too small"

  d_space :: (OA64_Word,OA64_Word) -> OA64_Word -> OA64_DataMem
  d_space (b,e) d = ((b,e),[(b,e,d)])

---------------------------------------------------------------------
-- Probes -----------------------------------------------------------

instance Probe (OA64_Cell OA64_Reg OA64_Word) where
   outp (Reg r (Val x)) = outp r ++ "=" ++ outp x
   outp (Reg r Inv) = outp r ++ "=Invalid"
   outp (Reg r Acid) = outp r ++ "=Exception"
   outp (Reg r Q) = outp r 
   outp (Loc w) = "PC:" ++ outp w
   outp (Imm w) = outp w
   outp (Comp n) = "Cluster " ++ outp n


instance Probe OA64_Op where
   outp (ALU (Add _)) = "+" 
   outp (ALU (Sub _)) = "-" 
   outp (ALU (Div _)) = "/" 
   outp (ALU (Mult _)) = "*" 
   outp (ALU Not) = "~" 
   outp (ALU And) = "&&" 
   outp (ALU Or) = "||" 
   outp (ALU (S LessThan)) = "<" 
   outp (ALU (S LessEqual)) = "<=" 
   outp (ALU (S GreaterThan)) = ">" 
   outp (ALU (S GreaterEqual)) = ">=" 
   outp (ALU (S Equal)) = "==" 
   outp (ALU (S NotEqual)) = "!=" 
   outp x = show x


instance Probe OA64_Reg
instance Probe OA64_Trans where
   outp (Trans _ FENCE _ i) = "FENCE" ++ outInfo i
   outp (Trans [] op [] i) = outp op ++  outInfo i
   outp (Trans [x] (COND op1 op2) [c,y,z,p] i)
       = outp x ++ " <- " ++ "(if0 " ++ outp c ++ " ("
            ++ outp op1 ++ "," ++ outp op2 ++ ")) "
            ++ outp y  ++ " " ++ outp z ++ " if " ++ outp p
            ++ outInfo i
   outp (Trans dummy STORE [c,y,z,p] i)
       = outp STORE ++" "++ outp c ++"("++ outp y ++") <- "
         ++  outp z ++ " if " ++ outp p ++ outInfo i 
   outp (Trans [o] op [x,y,p] i)
       = outp o ++ " <- " ++ outp x ++ " " ++ outp op ++ " " ++ outp y
                          ++ " if " ++ outp p
                          ++ outInfo i
   outp (Trans [o,o'] op [x,y,p] i)
       = "(" ++ outp o++ "," ++ outp o' ++ ")" ++ " <- " ++ 
             outp x ++ " " ++ outp op ++ " " ++ outp y
                          ++ " if " ++ outp p
                          ++ outInfo i
   outp (Trans [] op l i) = outp op ++" "++ outList l ++ outInfo i
   outp (Trans [o] op l i)
       = outp o ++ " <- " ++ outp op ++" "++ outList l ++ outInfo i
   outp (Trans l1 op l2 i)
       = outList l1 ++" <- "++ outp op ++" "++ outList l2 ++ outInfo i

outInfo (Loc x:Loc y:t) = outInfo' (Loc y:t)
outInfo x = outInfo' x

outInfo' [] = ""
outInfo' l = "  {" ++ foldr1 (\x y -> x ++ "," ++ y) (map outp l) ++ "}"

outList [] = ""
outList [x] = outp x
outList l = "[" ++ foldr1 (\x y -> x ++ "," ++ y) (map outp l) ++ "]"

