\begin{code}
module Components
  (
         bypassRF
	,fetchDLX
  )  where

import List
import Hawk
import qualified Trans as T
import TransSig

import DLX
\end{code}

Components Signature
<P> 
<HPPTAG getstr pre_on>
bypassRegFile :: (Signal DLXTrans,Signal DLXTrans) -> Signal DLXTrans
<HPPTAG getstr pre_off>

\begin{code}
fetchDLX x = instrFetch 4 dlx2trans x
\end{code}

<!--
bypassRegFile :: (Signal Trans,	-- instruction
		       Signal Trans)	-- writeback info
	              ->
	              Signal Trans	-- updated instruction
bypassRegFile (instr,writeback)
  = updSrc  instr initSrcValues
    where
      -- Create an auxilliary transaction with filled-in destination
      --  Cells. This transaction will be used as a bypass to update the
      --  source operands of the decoded instruction transaction.
      initSrcValues = lift4 (\regA valA regB valB -> [Reg regA (Val valA),
						     Reg regB (Val valB)])
			   regA regContentsA regB regContentsB
      (regA,regB) = unbundle2 $ map getSourceRegs instr
      (regContentsA,regContentsB) = registers regA regB writeBackData
      writeBackData = getDestRegData writeback

      getSourceRegs :: DLXTrans -> (DLXReg,DLXReg)
      getSourceRegs (Trans _ _ srcs info)
	= case (filter isReg srcs) of
	    []					-> (R0,R0)
	    [Reg regA _ ]			-> (regA,R0)
	    [Reg regA _, Reg regB _ ] -> (regA,regB)

      --getDestRegData :: Signal DLXTrans -> (Signal Word,Signal DstReg)
      getDestRegData trans
	= map (map extractData) $ filterDst isReg trans
	  where
	    extractData (Reg reg (Val val):t)
	      = (val,reg)
	    extractData []
	      = (0,R0)
	    extractData destCells
	      = error ("extractData -- destCells ill-formed: " ++ show destCells)
-->

\begin{code}
bypassRF instr wb = updSrc instr vals
   where vals = lift6 build r1 v1 v1' r2 v2 v2'
         build  r1 v1 v1' r2 v2 v2' = [Reg r1 (if r1==PC then v1' else Val v1)
                                      ,Reg r2 (if r2==PC then v2' else Val v2)
                                      ]
         (v1,v2) = registers r1 r2 wbdata
         wbdata = getDstData wb
         (v1',v2') = unbundle2 $ map getSrcVals instr
         (r1,r2) = unbundle2 $ map getSrcRegs instr

         getSrcVals (Trans _ _ srcs info)
                    = case (filter isReg srcs) of
                        []        -> (NotKnown,NotKnown)
                        [Reg _ x ]  -> (x,NotKnown)
                        [Reg _ x, Reg _ y] -> (x,y)


         getSrcRegs (Trans _ _ srcs info)
                    = case (filter isReg srcs) of
                        []        -> (R0,R0)
                        [Reg x _ ]  -> (x,R0)
                        [Reg x _, Reg y _ ] -> (x,y)

         getDstData t = map unzip $ map (map extractData) $ filterDst isReg t
                where extractData (Reg reg (Val val)) = (val,reg)
                      extractData (Reg Dummy _) = (0,R0)
                      extractData destCells = error ("extractData: " ++ show destCells)
 
\end{code} 





