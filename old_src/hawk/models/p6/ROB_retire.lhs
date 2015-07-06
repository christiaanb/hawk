<H3>ROB_retire.hs for P6 Case Study</H3>

\begin{code}
module ROB_retire(retire)  where

import Monad
import LazyST
import Hawk
import Trans

import qualified RF
import qualified AQ as Q
import qualified RAT 
import RF(RF)
import AQ(AQ)
import RAT(RAT)

import DLX
import Utils
\end{code}


\begin{code}
retire rat q regfile
  = do { perhaps <- retireable q
       ; let (retired,missed) = jumpHazard perhaps
       ; mapM (writeOut regfile rat) retired
       ; return (cleanUp retired,missed)
       }
  where cleanUp = map removeVirtuals
\end{code}

The type signature of <HPPTAG getstr tt_on>retireable<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
retireable :: Register r => AQ s (Trans i r w) -> ST s [Trans i r w]
<HPPTAG getstr pre_off>
\begin{code}
retireable q = Q.deQueueWhile q complete
\end{code}

The type signature of <HPPTAG getstr tt_on>jumpHazard<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
jumpHazard :: (Register r,Word w) => [VTrans r w] -> ([VTrans r w],Bool)
<HPPTAG getstr pre_off>
\begin{code}
jumpHazard [] = ([],False)
jumpHazard (instr:is) 
  = if branchMissed instr then ([instr],True)
       else (instr:is',False || die)
  where (is',die) = jumpHazard is
\end{code}

The type signature of <HPPTAG getstr tt_on>branchMissed<HPPTAG getstr tt_off> 
is:
<HPPTAG getstr pre_on>
branchMissed :: (Register r,Word w) => VTrans r w -> Bool
<HPPTAG getstr pre_off>
\begin{code}
branchMissed t = 
  do { Reg (Virtual _ (Just pc)) (Val x) <- getDstPC t
     ; Reg (Real spc) (Val y) <- getSpecPC t
     ; guard $ ispc pc
     ; guard $ isspecpc spc
     ; return $ x /= y
     }
   `catchEx` False
\end{code}

The type signature of 
<HPPTAG getstr tt_on>writeOut<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
writeOut :: Register r => RF s r w -> RAT s r Int -> 
                          VTrans r w -> ST s ()
<HPPTAG getstr pre_off>
\begin{code}
writeOut file rat t
  = do { let [Reg (Virtual vr (Just real)) (Val x)] = getDst t
       ; RF.write file real x
       ; a <- RAT.read rat real
       ; do {v <- a
            ; guard $ v == vr
            ; return $ RAT.remove rat real
            }
         `catchEx` return ()
       }
\end{code}



