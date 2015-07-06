<H3>Processor.lhs for P6 Case Study</H3>

\begin{code}
module Processor where
\end{code}

\begin{code}
import Hawk
import Trans 
import qualified TransSig as T
import qualified PreludeSig as Signaled
import Word

import Utils
import DLX

import Predict

import IFU(ifu)
import RS(rs)
import ROB(rob)
import EUs
\end{code}

The processor takes three arguments: a program (a list
of DLX transactions), program data (??), and an initial
program counter.  The processor produces a stream of 
retired instructions.
<P>
\begin{code}
processor ((pgm,pgmdata),startingPC) =  retired -- retired
  where
      (instrs,pc') = ifu (5,pgm) pc ([5,5] `before` space)
      --instrs' = probe "UFO" instrs

      --testTrans = lift1 (\n -> [pcTrans n]) space

      pc   = delay (pcTrans startingPC) npc
      npc = if' miss then' (Signaled.last retired ) 
                     else' pc'

      annotated = delay [] (
                    if' miss then' (lift0 []) 
                             else' (annotate $ filterOut isNop $ instrs)
                    )

      (retired,ready,space,miss) = rob 100 (annotated, computed)
      --miss' = if' miss then' (lift0 $ pcTrans 1) else' (lift0 $ pcTrans 0)
  
      computed = rs (150,execUnits) (delay False miss, delay [] ready)
\end{code}
The stream of retired instructions is produced by a
circuit that is described using a set of equations.


The above processor specification is parameterised by a list of
execution units:
\begin{code}
execUnits :: Word a => [EU DLX_Op (DLX_Cell (Virtual DLXReg Int) a)]
execUnits = [addUnit,addUnit,subUnit,jumpUnit,jumpUnit,multUnit,divUnit,cmpUnit,moveUnit]
\end{code}

The multiplication unit in the list of execution units could be replaced
by a probed unit.  The following function, which could replace
<TT>multUnit</TT> in the above definition,  puts a probe on the two
inputs to the multiplication unit and the output of the unit.  
The probe function takes a name (a string)
and the signal to probe.  These functions can be thought of as units
that output their inputs but have the side effect of printing strings
showing the values on these wires in each cycle.

\begin{code}
multUnit' b s = probe "mu_out" out
        where b' = probe "mu_cnt" b
              s' = probe "mu_in" s
              out = multUnit b' s'
\end{code}
