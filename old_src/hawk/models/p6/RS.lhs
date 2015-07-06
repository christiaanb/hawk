<H3>RS.hs for P6 Case Study</H3>

\begin{code}
module RS
  (
	rs
  )  where

import LazyST

import Hawk
import qualified PreludeSig as Sig
import Trans
import qualified TransSig as T
import qualified BoundedSet as Set

import EUs
import DLX
\end{code}

\begin{code}
type RS i c r w = (Int,[EU i (c (VReg r) w)]) ->
            (Signal Bool,Signal [VTrans r w]) -> 
            Signal [VTrans r w]
\end{code}

The type signature of <HPPTAG getstr tt_on>rs<HPPTAG getstr tt_off> is:
<HPPTAG getstr pre_on>
rs :: (Register r,Word w) => RS StandardOp c r w
<HPPTAG getstr pre_off>

\begin{code}
rs (n,execUnits) (mispredicted,input)
  = computed
   where
   ready = runST (
        do { set <- Set.new n
           ; loop wires $ 
                \(instrs,mispredicted,computed,rejected) -> 
                           if mispredicted 
                             then do { Set.clear set
                                     ; return []
                                     }
                             else do { Set.insert set instrs
                                     ; Set.insert set rejected
                                     ; broadcast' set computed
                                     ; ready <- Set.rmSuch set isComputable
                                     ; return ready
                                     }
          }
        )
   wires = bundle4 (input,mispredicted,computed,rejected)
   (computed,rejected) = unbundle2 $ delay ([],[]) $ 
                         execUnit mispredicted ready

   execUnit = schedule execUnits
\end{code}

\begin{code}
broadcast' set computed
  = do { s <- Set.read set
       ; let dests = concat $ map getDst computed
       ; Set.iterateSet set (flip fillInSrcCells dests)
       }
\end{code}
