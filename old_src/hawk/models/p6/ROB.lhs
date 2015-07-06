<H3>ROB.hs for P6 Case Study</H3>

\begin{code}
module ROB(rob)  where
import LazyST

import Hawk
import Trans

import qualified RF 
import qualified AQ as Q
import qualified RAT 
import RF(RF)
import AQ(AQ)
import RAT(RAT)
import ROB_insert(insert)
import ROB_retire(retire)

import DLX
\end{code}


<!--
type ROB s i r w =  Int ->                                         
              (s [Trans i r w], s [VTrans r w]) ->
              (s [Trans i r w], s [VTrans r w], s Int,s Bool)
-->
 
The type signature of <HPPTAG getstr tt_on>rob<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
rob :: (Signal s,Register r,Word w) => ROB s StandardOp r w
<HPPTAG getstr pre_off>

\begin{code}
rob n (fetched,computed)
  = unbundle4 $ runST (
    do { q <- Q.new n
       ; rat <- RAT.new
       ; regfile <- RF.new 
       ; loop(bundle2 (fetched,computed)) $ \ ~(fetched,computed) -> do
           { update q computed
           ; instrs <- insert rat q regfile fetched
           ; (retired,missed) <- retire rat q regfile
           ; inCase missed $ do { Q.clear q
                                ; RAT.clear rat
                                }
           ; capacity <- Q.space q
           ; let ready = if missed then [] else instrs
           ; return (retired,ready,capacity,missed)
           }
       }
   )   
    where 
    inCase x y = if x then y else return ()
\end{code}


<!-- assumes single register dest ops (not a good assumption) -->

The type signature of <HPPTAG getstr tt_on>update<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
update :: (Register r,Word w) => AQ s (VTrans r w) -> [VTrans r w] -> ST s ()
<HPPTAG getstr pre_off>

\begin{code}
update q
  = mapM_ $ \t ->
    do { let [Reg (Virtual v (Just r)) val] = getDst t 
       ; Q.insert q v t
       }
\end{code}

