<H3>IFU.lhs for P6 Case Study</H3>

\begin{code}
module IFU where

import LazyST
import Ix

import Trans
import Hawk
import DLX

import qualified PreludeSig as Signaled

import Word
\end{code}


\begin{code}
ifu (a,b) c d = (unique',id) >< fetch (4,dlx2trans,a,b) c d
\end{code}

\begin{code}
--unique' ts = ts
unique' ts = runST (
  do { v <- newSTRef 1
     ; loop(ts) $ \ ~ts -> do { mapM (\(Trans x y z i)  -> do { v' <- readSTRef v
                                  ; writeSTRef v (v'+1)
--                                  ; return $ Trans x y z (loc v':i)
                                  ; return $ Trans x y z (i ++ [loc v'])
                                  }) ts
                 }
     }
  )
\end{code}
