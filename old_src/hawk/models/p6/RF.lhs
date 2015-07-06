<H3>RF.lhs for P6 Case Study</H3>

\begin{code}
module RF where

import Prelude hiding (read)
import LazyST
import Utils

import Hawk
\end{code}

\begin{code}
type RF s a b = STArray s a b
\end{code}

\begin{code}
new   :: (Register a,Num b) => ST c (RF c a b)
\end{code}

\begin{code}
read  :: Register a => RF s a b -> a -> ST s b
\end{code}

\begin{code}
write :: Register a => RF s a b -> a -> b -> ST s ()
\end{code}

\begin{code}
new         = newSTArray (minBound,maxBound) 0
\end{code}

\begin{code}
read        = readSTArray 
\end{code}

\begin{code}
write f x z = if readOnly x then return () 
              else writeSTArray f x z
\end{code}
