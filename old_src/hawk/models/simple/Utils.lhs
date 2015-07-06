\begin{code}
module Utils where

import Hawk
import qualified Trans
import DLX
import Maybe
import TransSig
\end{code}

\begin{code}
mkTrans x = map (\s -> Trans.nop) x 
\end{code}

\begin{code}
isTrapTrans :: Word a => Signal (DLX_Trans a) -> Signal Bool
isTrapTrans = lift1 isTrap
   where isTrap (Trans.Trans [ _ , Reg IAR _ ] _ _ _ )    = True
         isTrap _                                              = False
\end{code}

\begin{code}
branchTaken trans
  = map not $ (lift1 null) $ filterDst (\c -> isPC c && isAss c) trans
\end{code}
 
