<H3>PipeUtils.lhs</H3>

\begin{code}
module PipeUtils where

import Hawk
import qualified Trans
import TransSig
import DLX
import Maybe
\end{code}

\begin{code}
mkTrans x = map (\x -> Trans.nop) x 
\end{code}

\begin{code}
isExecTrans trans
  = lift1 not (modifiesPC trans) *&& fmap isExec trans
    where
      isExec (Trans.Trans _ op _ _) = not (isNoOp op)
\end{code}

\begin{code}
modifiesPC trans
  = lift1 not $ fmap null $ filterDst isPC trans
\end{code}




\begin{code}
isTrapTrans :: Word a => Signal (DLX_Trans a) -> Signal Bool
isTrapTrans = lift1 isTrap
   where isTrap (Trans.Trans [ _ , Reg IAR _ ] _ _ _ )    = True
         isTrap _                                              = False
\end{code}

\begin{code}
printAssm :: Show a => Int -> [a] -> IO ()
printAssm n (x:xs)
  = do putStr (show n)
       putStr " : "
       putStrLn (show x)
       printAssm (n+1) xs
printAssm _ _ = return ()
\end{code}

 
\begin{code}
getLocation' t = liftEx $ Trans.getLoc t
\end{code}

\begin{code}
getDestPC' t = liftEx $ Trans.getDstPC t
\end{code}

\begin{code}
getSrcPC' t = liftEx $ Trans.getSrcPC t
\end{code}
 

