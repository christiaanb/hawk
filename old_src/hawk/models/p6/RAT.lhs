<H3>RAT.lhs for P6 Case Study</H3>

Module header:
\begin{code}
module RAT where
\end{code}

\begin{code}
import LazyST

import Prelude hiding (read)
import Hawk
import DLX
import qualified Trans
import Utils
\end{code}

\begin{code}
new      :: Register a => ST s (RAT s a b)
\end{code}
\begin{code}
write    :: Register a => RAT s a b -> a -> b -> ST s ()
\end{code}
\begin{code}
remove   :: Register a => RAT s a b -> a -> ST s ()
\end{code}
\begin{code}
read     :: Register a => RAT s a b -> a -> ST s (Maybe b)  
\end{code}
\begin{code}
clear    :: Register a => RAT s a b -> ST s ()
\end{code}

\begin{code}
type RAT s a b = (STArray s a (Maybe b),a,a)
\end{code}

\begin{code}
clear (xs,x1,x2)
  = do { mapM (\x -> writeSTArray xs x Nothing) [x1 .. x2]
       ; return ()
       }
\end{code}

\begin{code}
replace rat (Reg r x) 
  = do { a <- read rat r
       ; let res = do { v <- a
                      ; return (Reg (Virtual v (Just r)) x) 
                      } 
                   `catchEx` Reg (Real r) x
       ; return res
       }
replace rat x = return $ convert x
\end{code}

\begin{code}
new
  = do { x <- newSTArray (minBound,maxBound) Nothing
       ; return (x,minBound,maxBound)
       }
\end{code}

\begin{code}
write (xy,_,_) x y = writeSTArray xy x (return y)
\end{code}
\begin{code}
remove (xy,_,_) x = writeSTArray xy x Nothing
\end{code}
\begin{code}
read  (xy,_,_) x = readSTArray xy x
\end{code}
