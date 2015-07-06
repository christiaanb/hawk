<H3>Pipeline.lhs</H3>
<!-- \outline{\section*{Pipeline Specification}} -->

\begin{code}
module Pipeline(pipeline) where

import Hawk 
import PipePredict
import qualified Trans
import TransSig
import Components
import PipeUtils
import DLX
\end{code}

\begin{code}
pipeline ((pgm,pgmdata),startPC)  = writeback'
    where

      predictions  = btb 37 startPC instr  executed'
      
      pc = pipeReg (Trans.pcTrans startPC) pcLogic npc

      npc = if' (isBranch executed *&& mispredicted executed) 
            then' executed
            else' (if' (isBranch instr) 
                   then' predictions
                   else' pc')

      pcLogic = if' dataHazard then' stall else' input

      (instr,pc') = fetchDLX pgm pc

      instrA = if' (isBranch instr) 
               then' (annotate predictions instr)
               else' instr

      instr' = pipeReg Trans.nop ifLogic instrA

      ifLogic   = if' (mispredicted executed) then' kill
                  else' (if' dataHazard then' stall
                         else' input)

      regFetch = bypassRF  instr' writeback'

      regFetch' = pipeReg Trans.nop rfLogic regFetch
      
      rfLogic = if' (mispredicted executed *|| dataHazard) 
                          then' kill
                          else' input

      executed  = exec $ regFetch' `bypassMany` (bundleList [executed', writeback'])

      executed' = pipeReg Trans.nop input executed
      
      writeback  = mem 4 pgmdata (executed' `bypass` writeback')
      writeback' = probe "writeback2" $ 
                     pipeReg Trans.nop  input writeback
                           -- (trunc isTrapTrans writeback)
 
      dataHazard = execLoadHazard *|| branchExecHazard 

      execLoadHazard    = isLoad regFetch' *&&
                          isExecTrans regFetch     *&&
                          rawHazard (regFetch',regFetch)

      branchExecHazard   = isExecTrans regFetch' *&&
                           modifiesPC regFetch      *&&
                           rawHazard (regFetch',regFetch)
\end{code}
