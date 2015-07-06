\begin{code}
module PreludeSig where

import Prelude(Ord,Bool,Int,($),(.))
import qualified Prelude as P
import qualified List
import Signal
\end{code}

{- 
   Functions in this module are functions from the Haskell
   prelude lifted on Signals
-}

\begin{code}
last ::  Signal [a] -> Signal a
\end{code}

\begin{code}
head :: Signal [a] -> Signal a
\end{code}

\begin{code}
tail :: Signal [a] -> Signal [a]
\end{code}

\begin{code}
splitAt :: Signal Int -> Signal [a] -> (Signal [a], Signal [a])
\end{code}

\begin{code}
length :: Signal [a] -> Signal Int
\end{code}

\begin{code}
max :: Ord a => Signal a -> Signal a -> Signal a
\end{code}

\begin{code}
min :: Ord a => Signal a -> Signal a -> Signal a
\end{code}

\begin{code}
maximum :: Ord a => Signal [a] -> Signal a
\end{code}

\begin{code}
minimum :: Ord a => Signal [a] -> Signal a
\end{code}

\begin{code}
filter :: (a -> Bool) -> Signal ([a]) -> Signal ([a])
\end{code}

\begin{code}
partition :: (a -> Bool) -> Signal [a] -> (Signal [a],Signal [a])
\end{code}

\begin{code}
fst :: Signal (a,b) -> Signal a
\end{code}

\begin{code}
snd :: Signal (a,b) -> Signal b
\end{code}

\begin{code}
not :: Signal Bool -> Signal Bool
\end{code}

\begin{code}
last = lift1 P.last
\end{code}

\begin{code}
head = lift1 P.head
\end{code}

\begin{code}
tail = lift1 P.tail
\end{code}

\begin{code}
splitAt x y = unbundle2 $ lift2 P.splitAt x y
\end{code}

\begin{code}
length = lift1 P.length
\end{code}

\begin{code}
max = lift2 P.max
\end{code}

\begin{code}
min = lift2 P.min
\end{code}

\begin{code}
maximum = lift1 P.maximum
\end{code}

\begin{code}
minimum = lift1 P.minimum
\end{code}

\begin{code}
filter x y = lift1 (P.filter x) y
\end{code}

\begin{code}
partition x y = unbundle2 (lift1 (List.partition x) y )
\end{code}

\begin{code}
fst = lift1 P.fst
\end{code}

\begin{code}
snd = lift1 P.snd
\end{code}

\begin{code}
not = lift1 P.not
\end{code}
