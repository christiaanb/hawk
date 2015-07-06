<H3>ROB_insert.hs for P6 Case Study</H3>

\begin{code}
module ROB_insert(insert)  where

import Hawk
import Trans
import LazyST

import qualified RF 
import qualified AQ as Q
import qualified RAT
import AQ(AQ)
import RAT(RAT)
import RF(RF) 
import Utils


import DLX
\end{code}

The type signature for <HPPTAG getstr tt_on>insert<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
insert :: (Register r,Word w) => 
     RAT st r Int -> AQ st (VTrans r w) -> 
     RF st r w -> [Trans StandardOp r w] -> ST st [VTrans r w] 
<HPPTAG getstr pre_off>

\begin{code}
insert rat q regfile instrs 
  = mapM setAndSubst instrs
   where 
   setAndSubst t  =
       do { (reg,alias) <- bind q t
          ; src <- mapM (RAT.replace rat) $ getSrc t
          ; let info = map convert $ getInfo t
          ; let op = getOp t
          ; RAT.write rat reg alias
          ; dest <- mapM (RAT.replace rat) $ getDst t
          ; new <- regRead q regfile $ Trans dest op src info
          ; Q.enQueue q new
          ; return $ new
          }
   bind q trans = Q.assignAddr q (head . getDstRegs $ trans)
\end{code}

The type signature for <HPPTAG getstr tt_on>regRead<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
regRead :: (Register r,Word w) => AQ s (VTrans r w) -> RF s r w -> 
                         VTrans r w -> ST s (VTrans r w)
<HPPTAG getstr pre_off>

\begin{code}
regRead q file t  
  = do { cells <- mapM subst (getSrcRegs t)
       ; return $ fillInSrcCells t cells
       }
   where 
   subst (Virtual n x) 
     = do { v <- liftST $ Q.getQVal q n
          ; t <- liftEx v
          ; [Reg _ val] <- return $ getDst t
          ; return $ Reg (Virtual n x) val
          }
       `handle` (Reg (Virtual n x) NotKnown)
   subst (Real r) 
     = do { val <- RF.read file r
          ; return $ Reg (Real r) (Val val)
          }
\end{code}
