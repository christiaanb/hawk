<H3>Predict.lhs for P6 Case Study</H3>

\begin{code}
module Predict where

import Hawk
import Trans 
import qualified TransSig as T

import DLX
\end{code}

\begin{code}
annotate x = lift1 (map anno) x
  where anno t = let Just (Reg PC (Val pc)) = getSrcPC t 
                 in if isBranch t then addInfo (Reg SpecPC (Val pc)) t
                    else t
\end{code}

