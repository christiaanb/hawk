\begin{code}
module Trans where

import List
import Words
import Word
import Arithmetic
import Cell
import Memory
import Register
import Instruction
\end{code}


-- Begin Signature ----------------------------------------------------------
{-

  We have used Transactions to represent instructions w/ their data.
  These have been particularly useful in pipelined and out-of-order 
  superscalar machines.  

-}

\begin{code}
data Trans i c = Trans [c] i [c] [c]
		   deriving (Eq,Show,Read)
\end{code}


-- Convention: if Trans d op s i
-- we say that d is the destination, op is the instruction
-- s is the source, and i is the information

-- return a nop-like transaction
\begin{code}
nop             :: (Instruction i,Register r) => Trans i (c r w)
\end{code}


-- return a PC transaction
\begin{code}
pcTrans         :: (Cell c,Instruction i,Register r, Word w) => 
                       w -> Trans i (c r w)
\end{code}


\begin{code}
isNop           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isAdd           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isAlu           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isCmp           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isBool          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isSub           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isMul           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isDiv           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isJump          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isMove          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isMem           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isLoad          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isStore         :: (Instruction i,Register r) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isBranch        :: (Cell c,Register r,Word w) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
isComputable    :: (Cell c,Register r,Word w) => Trans i (c r w) -> Bool
\end{code}

-- update destination fields
\begin{code}
updDst          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}

-- apply a function to the destination fields
\begin{code}
repDst          :: Register r => (c r w -> c r w -> Bool) ->
                                 Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}

-- add to the destination
\begin{code}
addDst          :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)
\end{code}

-- get the destination
\begin{code}
getDst          :: Register r => Trans i (c r w) -> [c r w]
\end{code}

-- replace the dest fields
\begin{code}
putDst          :: Register r => Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}


\begin{code}
updSrc          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}

\begin{code}
addSrc          :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)
\end{code}

\begin{code}
getSrc          :: Register r => Trans i (c r w) -> [c r w]
\end{code}

\begin{code}
putSrc          :: Register r => Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}

\begin{code}
addInfo         :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)
\end{code}

\begin{code}
getInfo         :: Register r => Trans i (c r w) -> [c r w]
\end{code}

\begin{code}
putInfo         :: Register r => Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}

\begin{code}
getOp           :: Trans i (c r w) -> i
\end{code}

\begin{code}
putOp           :: Trans i (c r w) -> i -> Trans i (c r w)
\end{code}



-- return the speculative PC from the info area
\begin{code}
getSpecPC       :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)
\end{code}

-- return the PC from the destination area
\begin{code}
getDstPC        :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)
\end{code}
\begin{code}
getSrcPC        :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)
\end{code}

-- return the instructions location from memory from the destination
-- area
\begin{code}
getLoc          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)
\end{code}

-- get register references
\begin{code}
getSrcRegs      :: (Cell c,Register r,Word w) => Trans i (c r w) -> [r]
\end{code}
\begin{code}
getDstRegs      :: (Cell c,Register r,Word w) => Trans i (c r w) -> [r]
\end{code}

-- get register reference values
\begin{code}
getSrcRegVals   :: (Cell c,Register r,Word w) => Trans i (c r w) -> [w]
\end{code}
\begin{code}
putDstRegVal    :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> w -> Trans i (c r w)
\end{code}

-- evalTrans t (c,w) update the destination fields in t with w if they match
-- c
\begin{code}
evalTrans       :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> (c r w,Maybe w) -> Trans i (c r w)

-- is there a Read-After-Write hazard between two transactions?
\end{code}
\begin{code}
rawHazard       :: (Cell c,Register r,Word w) => 
                   (Trans i (c r w),Trans i (c r w)) -> Bool
\end{code}

-- bypass t t2 source operands of t with the dest operands of t2
-- if the references match.

\begin{code}
bypass          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Trans i (c r w) -> Trans i (c r w)

\end{code}
-- bypass the dest. operands instead of the source operands.
\begin{code}
bypassDst       :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Trans i (c r w) -> Trans i (c r w)
\end{code}

-- bypass with multiple transactions
\begin{code}
bypassMany      :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [Trans i (c r w)] -> Trans i (c r w)
\end{code}
\begin{code}
bypassDstMany   :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [Trans i (c r w)] -> Trans i (c r w)
\end{code}

-- bypass to multiple transaction with multiple transactions
\begin{code}
broadcast       :: (Cell a, Register b, Word c) => 
                   [Trans e (a b c)] -> [Trans e (a b c)] -> [Trans e (a b c)]
\end{code}

--source operands and dest operands all filled in?
\begin{code}
complete        :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Bool
\end{code}

-- if (x,y) = readyToRetire z, then
-- x is the lift of transactions that are "complete"
\begin{code}
readyToRetire   :: (Cell c,Register r,Word w) => 
                   [Trans i (c r w)] -> ([Trans i (c r w)],[Trans i (c r w)])
\end{code}


-- if (x,y) = readyToCompute z, then
-- x is the lift of transactions with all of their source operands filled in
\begin{code}
readyToCompute  :: (Cell c,Register r,Word w) => 
                   [Trans i (c r w)] -> ([Trans i (c r w)],[Trans i (c r w)])
\end{code}


\begin{code}
updatePC        :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)
\end{code}


\begin{code}
getPredicate   :: (Cell c,Register r,Word w) => Trans i (c r w) -> c r w
\end{code}
\begin{code}
isPredicated   :: (Cell c,Register r,Word w) => Trans i (c r w) -> Bool
\end{code}

\begin{code}
evalPredicate  :: (Cell c,Register r,Word w) => Trans i (c r w) -> w
\end{code}
-- End Signature ----------------------------------------------------------


\begin{code}
updCells        :: (Cell c,Register r,Word w) => [c r w] -> [c r w] -> [c r w]
\end{code}

\begin{code}
repCells        :: Register r => (c r w -> c r w -> Bool) ->
                                  [c r w] -> [c r w] -> [c r w]
\end{code}


-- perhaps these functions can go?
\begin{code}
filterDst       :: Register r => (c r w -> Bool) -> Trans i (c r w) -> [c r w]
\end{code}


\begin{code}
fillInCells     :: (Cell c,Register r,Word w) => [c r w] -> [c r w] -> [c r w]
\end{code}


\begin{code}
fillInSrcCells  :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [c r w] -> Trans i (c r w)
\end{code}


\begin{code}
filterOut       :: (Register r,Functor m) => 
                   (Trans i (c r w) -> Bool) -> m [Trans i (c r w)] -> 
                   m [Trans i (c r w)]
\end{code}




\begin{code}
nop = Trans [] noOp [] []
\end{code}


\begin{code}
pcTrans addr = Trans [putVal pcNothing (Just addr)] noOp [] []
\end{code}


\begin{code}
isNop t = isNoOp (getOp t)
\end{code}


\begin{code}
isAdd t = isAddOp (getOp t)
\end{code}


\begin{code}
isAlu t = isAluOp (getOp t)
\end{code}


\begin{code}
isCmp t = isCmpOp (getOp t)
\end{code}


\begin{code}
isBool t = isBoolOp (getOp t)
\end{code}


\begin{code}
isSub t = isSubOp (getOp t)
\end{code}


\begin{code}
isMul t = isMultOp (getOp t)
\end{code}


\begin{code}
isDiv t = isDivOp (getOp t)
\end{code}


\begin{code}
isJump t = isJumpOp (getOp t)
\end{code}


\begin{code}
isMem t = isMemOp (getOp t)
\end{code}


\begin{code}
isMove t = isMoveOp (getOp t)
\end{code}


\begin{code}
isLoad t = isLoadOp (getOp t)
\end{code}


\begin{code}
isStore t = isStoreOp (getOp t)
\end{code}


\begin{code}
isBranch (Trans d _ _ _) = any search d where
        search r = if isReg r then ispc (getReg r)
                   else False
\end{code}


\begin{code}
isComputable = and . map isComputed . getSrc
\end{code}





\begin{code}
repCells replFunc cells replacements
  = map (\cell -> foldr bypassCell cell replacements) cells
    where
      bypassCell bypassed argCell
	= if replFunc bypassed argCell
	    then bypassed
	    else argCell
\end{code}



\begin{code}
updCells cells bypassCells = repCells cellHazard cells bypassCells
\end{code}



\begin{code}
repDst repFunc (Trans d o s i) cells = Trans (repCells repFunc d cells) o s i
\end{code}

\begin{code}
updDst = repDst cellHazard
\end{code}

\begin{code}
addDst c t = putDst t (c:getDst t)
\end{code}

\begin{code}
getDst (Trans d o s i) = d
\end{code}

\begin{code}
putDst (Trans _ o s i) d = Trans d o s i
\end{code}


\begin{code}
updSrc (Trans d o s i) cells = Trans d o (updCells s cells) i
\end{code}

\begin{code}
addSrc c t = putSrc t (c:getSrc t)
\end{code}

\begin{code}
getSrc (Trans d o s i) = s
\end{code}

\begin{code}
putSrc (Trans d o _ i) s = Trans d o s i
\end{code}


\begin{code}
addInfo c t = putInfo t (c:getInfo t)
\end{code}

\begin{code}
getInfo (Trans d o s i) = i
\end{code}

\begin{code}
putInfo (Trans d o s _) i = Trans d o s i 
\end{code}


\begin{code}
getOp (Trans d o s i) = o
\end{code}

\begin{code}
putOp (Trans d _ s i) o = Trans d o s i
\end{code}


\begin{code}
getSpecPC	= find isSpecPC . getInfo
\end{code}

\begin{code}
getDstPC	= find isPC . getDst
\end{code}

\begin{code}
getSrcPC	= find isPC . getSrc
\end{code}

\begin{code}
getLoc		= find isLoc . getInfo
\end{code}

\begin{code}
getSrcRegs t = map getReg $ filter isReg $ getSrc t
\end{code}
\begin{code}
getDstRegs t = map getReg $ filter isReg $ getDst t
\end{code}

\begin{code}
getSrcRegVals t = map getVal $ 
			filter isReg $ getSrc t
\end{code}

{-
putDstRegVal (Trans [Reg r _] o s i) n
			= Trans [Reg r (Val n)] o s i
-}
\begin{code}
putDstRegVal (Trans [r] o s i) n
			= Trans [putVal r (Just n)] o s i
\end{code}

\begin{code}
getPredicate (Trans _ _ l _) = last (filter isPred l)
\end{code}

\begin{code}
getPredicate' t = if isPredicated t then Just (getPredicate t)
                  else Nothing
\end{code}

\begin{code}
isPredicated (Trans _ _ x _) 
    = case filter isPred  x of
		[] -> False
                _ -> True
\end{code}


\begin{code}
evalPredicate t =
   case getPredicate' t of
              Just c -> if isAss c then getVal c
                   --     else error $ "evalPredicate" ++ show t
                        else error "evalPredicate" 
              Nothing -> 1
\end{code}

\begin{code}
bypass tran bypassT = --updSrc tran $ getDst bypassT
                      if evalPredicate bypassT /= 0
                          then updSrc tran $ getDst bypassT
                          else tran
\end{code}

\begin{code}
bypassDst tran bypassT = if evalPredicate bypassT /= 0 
                            then updDst tran $ getDst bypassT
                            else tran
\end{code}

\begin{code}
bypassMany tran bypassT = foldr (\a b -> b `bypass` a) tran bypassT
\end{code}

\begin{code}
bypassDstMany tran bypassT = foldr (\a b -> b `bypassDst` a) tran bypassT
\end{code}

\begin{code}
broadcast xs ys = map (`bypassMany` ys) xs
\end{code}

{- PRE-predication
bypass tran bypassT = updSrc tran $ getDst bypassT

bypassDst tran bypassT = updDst tran $ getDst bypassT

bypassTrans tran bypassT = foldr (\a b -> b `bypass` a) tran bypassT

broadcast xs ys = map (`bypassTrans` ys) xs
-}


\begin{code}
readyToRetire  = partition $ and . map isComputed . getDst
\end{code}

\begin{code}
complete = and . map isComputed . getDst
\end{code}

\begin{code}
readyToCompute =  partition $ and . map isComputed . getSrc
\end{code}

\begin{code}
tran `evalTrans` (dest,val) = repDst sameLoc tran [putVal dest val]
\end{code}

\begin{code}
rawHazard (preceeding,following)
  = or [ cellHazard precCell followCell |
	   precCell <- getDst preceeding,
	   followCell <- getSrc following]
\end{code}

\begin{code}
filterDst f (Trans d _ _ _) = filter f d
\end{code}

--added 19 Nov
\begin{code}
filterOut f = fmap (filter $ not . f)
\end{code}

\begin{code}
fillInCells cells bypassCells
  = repCells (\x y -> (not $ isAss y) && cellHazard x y) cells bypassCells
\end{code}
  
\begin{code}
fillInSrcCells (Trans d o s i) cells = Trans d o (fillInCells s cells) i
\end{code}

\begin{code}
fillInCells' cells bypassCells
  = repCells cellHazard cells bypassCells
\end{code}
  
\begin{code}
fillInSrcCells' (Trans d o s i) cells = Trans d o (fillInCells' s cells) i
\end{code}
  
-- TEMPORARY --- NOT ROBUST!
\begin{code}
updatePC c (Trans _ o s i) = Trans [c] o s i
\end{code}









