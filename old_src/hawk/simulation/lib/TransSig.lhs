\begin{code}
module TransSig
  (     Trans(..)

	,nop

	,isNop
	,isAdd
	,isAlu
	,isCmp
	,isBool
	,isSub
	,isMul
	,isDiv
	,isJump
	,isMem
	,isMove
	,isLoad
	,isStore
	,isBranch

	,updCells
	,repCells

	,updDst
        ,repDst
        ,addDst
	,getDst
        ,putDst

	,updSrc
        ,addSrc
        ,getSrc
        ,putSrc

	,addInfo
        ,getInfo
        ,putInfo

        ,getOp
	,putOp

	,getSpecPC
	,getDstPC
	,getSrcPC
        ,getLoc 

        ,getSrcRegs 
        ,getDstRegs
        ,getSrcRegVals 
        ,putDstRegVal

        ,bypass
	,bypassDst
	,bypassMany
	,bypassDstMany
	,broadcast

	,readyToRetire 
	,complete
	,readyToCompute

	,evalTrans
        ,rawHazard
	,filterDst

        ,pcTrans
        , getPredicate
        , isPredicated 
        , evalPredicate



  ) where

import List
import Instruction
import Signal
import Register
import Words
import Arithmetic
import qualified Trans as T
import Trans(Trans(..))
import Cell
\end{code}


-- Begin Signature ---------------------------------------------------
{- 
  The functions in TransSig are identical to Trans, except
  that they have been lifted on Signals
-}


\begin{code}
nop		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
isNop		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isAdd		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isAlu		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isCmp		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isBool		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isSub		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isMul		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isDiv		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isJump		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isMem		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isMove		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isLoad		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isStore		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
isBranch	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}
\begin{code}
updCells	:: (Cell c, Word w, Register r) => 
                    Signal [c r w] -> Signal [c r w] -> Signal [c r w]
\end{code}
\begin{code}
repCells	:: (Cell c, Word w, Register r) => 
                    (c r w -> c r w -> Bool) -> 
		    Signal [c r w] -> Signal [c r w] -> Signal [c r w]

\end{code}
\begin{code}
updDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
repDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   (c r w -> c r w -> Bool) ->
		   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
addDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (c r w) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
getDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w]
\end{code}
\begin{code}
putDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
updSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
addSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (c r w) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
getSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w]
\end{code}
\begin{code}
putSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))

\end{code}
\begin{code}
addInfo		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (c r w) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
getInfo		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w]
\end{code}
\begin{code}
putInfo		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))

\end{code}
\begin{code}
getOp		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal i
\end{code}
\begin{code}
putOp		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal i -> 
                   Signal (Trans i (c r w))

\end{code}
\begin{code}
getSpecPC	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))
\end{code}
\begin{code}
getDstPC	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))
\end{code}
\begin{code}
getSrcPC	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))
\end{code}
\begin{code}
getLoc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))

\end{code}
\begin{code}
getSrcRegs	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [r]
\end{code}
\begin{code}
getDstRegs	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [r]
\end{code}
\begin{code}
getSrcRegVals	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [w]
\end{code}
\begin{code}
putDstRegVal	:: (Instruction i,Cell c,Register r,Word w) =>  
                   Signal (Trans i (c r w)) -> Signal w -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
bypass		:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
\end{code}
\begin{code}
bypassDst	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal (Trans i (c r w)) ->
	           Signal (Trans i (c r w))
\end{code}
\begin{code}
bypassMany	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal [Trans i (c r w)] ->
		   Signal (Trans i (c r w))
\end{code}
\begin{code}
bypassDstMany	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal [Trans i (c r w)] ->
		   Signal (Trans i (c r w))
\end{code}
\begin{code}
broadcast ::       (Cell a, Register b, Word c) => 
                   Signal [Trans e (a b c)] -> Signal [Trans e (a b c)] -> 
                   Signal [Trans e (a b c)]
\end{code}


\begin{code}
readyToRetire	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal [Trans i (c r w)] -> 
                   Signal ([Trans i (c r w)],[Trans i (c r w)])
\end{code}

\begin{code}
complete	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
\end{code}

\begin{code}
readyToCompute	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal [Trans i (c r w)] -> 
                   Signal ([Trans i (c r w)],[Trans i (c r w)])
\end{code}

\begin{code}
evalTrans	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (c r w, Maybe w) -> 
                   Signal (Trans i (c r w))
\end{code}

\begin{code}
rawHazard	:: (Instruction i,Cell c,Register r,Word w) => 
                   (Signal (Trans i (c r w)),Signal (Trans i (c r w))) 
                   -> Signal Bool
\end{code}
\begin{code}
filterDst	:: (Instruction i,Cell c,Register r,Word w) => 
                   (c r w -> Bool) -> Signal (Trans i (c r w)) -> 
	           Signal [c r w]
\end{code}

\begin{code}
pcTrans         :: (Instruction i,Cell c,Register r,Word w) => 
                   Signal w -> Signal (Trans i (c r w))
\end{code}

\begin{code}
getPredicate   :: (Cell c,Register r,Word w) => 
                  Signal (Trans i (c r w)) -> Signal (c r w)
\end{code}

\begin{code}
isPredicated   :: (Cell c,Register r,Word w) => 
                  Signal (Trans i (c r w)) -> Signal Bool
\end{code}


\begin{code}
evalPredicate   :: (Cell c,Register r,Word w) => 
                  Signal (Trans i (c r w)) -> Signal w
\end{code}



-- End Signature -------------------------------------------------------------
\begin{code}
nop 			= lift0 $ T.nop
\end{code}


\begin{code}
isNop			= lift1 T.isNop
\end{code}

\begin{code}
isAdd			= lift1 T.isAdd
\end{code}

\begin{code}
isAlu			= lift1 T.isAlu
\end{code}

\begin{code}
isCmp			= lift1 T.isCmp
\end{code}

\begin{code}
isBool			= lift1 T.isBool
\end{code}

\begin{code}
isSub			= lift1 T.isSub
\end{code}

\begin{code}
isMul			= lift1 T.isMul
\end{code}

\begin{code}
isDiv			= lift1 T.isDiv
\end{code}

\begin{code}
isJump			= lift1 T.isJump
\end{code}

\begin{code}
isMem			= lift1 T.isMem
\end{code}

\begin{code}
isMove			= lift1 T.isMove
\end{code}

\begin{code}
isLoad			= lift1 T.isLoad
\end{code}

\begin{code}
isStore			= lift1 T.isStore
\end{code}

\begin{code}
isBranch		= lift1 T.isBranch
\end{code}


\begin{code}
updCells		= lift2 T.updCells
\end{code}

\begin{code}
repCells f		= lift2 $ T.repCells f
\end{code}


\begin{code}
updDst	 		= lift2 T.updDst
\end{code}

\begin{code}
repDst f		= lift2 $ T.repDst f
\end{code}

\begin{code}
addDst			= lift2 T.addDst
\end{code}

\begin{code}
getDst			= lift1 T.getDst
\end{code}

\begin{code}
putDst			= lift2 T.putDst
\end{code}


\begin{code}
updSrc			= lift2 T.updSrc
--repSrc		=
\end{code}

\begin{code}
addSrc			= lift2 T.addSrc
\end{code}

\begin{code}
getSrc			= lift1 T.getSrc
\end{code}

\begin{code}
putSrc			= lift2 T.putSrc
\end{code}


\begin{code}
addInfo			= lift2 T.addInfo
\end{code}

\begin{code}
getInfo			= lift1 T.getInfo
\end{code}

\begin{code}
putInfo			= lift2 T.putInfo
\end{code}


\begin{code}
getOp			= lift1 T.getOp
\end{code}

\begin{code}
putOp			= lift2 T.putOp
\end{code}


\begin{code}
getSpecPC		= lift1 T.getSpecPC
\end{code}

\begin{code}
getDstPC		= lift1 T.getDstPC
\end{code}

\begin{code}
getSrcPC		= lift1 T.getSrcPC
\end{code}

\begin{code}
getLoc			= lift1 T.getLoc
\end{code}


\begin{code}
getSrcRegs		= lift1 T.getSrcRegs
\end{code}

\begin{code}
getDstRegs		= lift1 T.getDstRegs
\end{code}

\begin{code}
getSrcRegVals		= lift1 T.getSrcRegVals
\end{code}

\begin{code}
putDstRegVal		= lift2 T.putDstRegVal
\end{code}


\begin{code}
evalTrans		= lift2 T.evalTrans
\end{code}

\begin{code}
rawHazard ts		= lift1 T.rawHazard (bundle2 ts)
\end{code}


\begin{code}
bypass			= lift2 T.bypass
\end{code}

\begin{code}
bypassDst		= lift2 T.bypassDst
\end{code}

\begin{code}
bypassMany		= lift2 T.bypassMany
\end{code}

\begin{code}
bypassDstMany		= lift2 T.bypassDstMany
\end{code}

\begin{code}
broadcast		= lift2 T.broadcast
\end{code}

\begin{code}
readyToRetire		= lift1 T.readyToRetire
\end{code}

\begin{code}
complete		= lift1 T.complete
\end{code}

\begin{code}
readyToCompute		= lift1 T.readyToCompute
\end{code}


\begin{code}
filterDst f		= lift1 $ T.filterDst f
\end{code}



\begin{code}
pcTrans = lift1 T.pcTrans
\end{code}


-- Predicated instructions
\begin{code}
getPredicate = lift1 T.getPredicate
\end{code}

\begin{code}
isPredicated = lift1 T.isPredicated
\end{code}

\begin{code}
evalPredicate = lift1 T.evalPredicate
\end{code}
