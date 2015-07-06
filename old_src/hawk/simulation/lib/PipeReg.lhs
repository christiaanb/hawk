\begin{code}
module PipeReg where


import Trans
import Signal 
import Register
import Instruction
\end{code}

-- Begin Signature ----------------------------------------------------------

{- 
  pipeReg is helpful for constructing in pipelines 
-}

\begin{code}
data PipeRegCmd = Input | Stall | Kill
                  deriving (Eq,Ord,Enum,Bounded,Show)
\end{code}


-- pipeReg t cmd ts , on the first cycle return "t", in later cycles,
-- if cmd=Input then return ts, if cmd=Stall then return the previous
-- value and store the input, if cmd=Kill then return a nop

\begin{code}
pipeReg :: (Instruction a, Register b) => 
           Trans a (c b d) -> Signal PipeRegCmd -> 
           Signal (Trans a (c b d)) -> Signal (Trans a (c b d))
\end{code}

\begin{code}
input           :: Signal PipeRegCmd
\end{code}

\begin{code}
stall           :: Signal PipeRegCmd
\end{code}

\begin{code}
kill            :: Signal PipeRegCmd
\end{code}

-- End Signature ----------------------------------------------------------

\begin{code}
pipeReg init cmd incoming = out
  where out = delay init (if' (cmd*==input) then' incoming
                          else' (if' (cmd*==stall) then' out 
                          else' (lift0 nop))
                         )
\end{code}

\begin{code}
input = lift0 Input
\end{code}

\begin{code}
stall = lift0 Stall
\end{code}

\begin{code}
kill = lift0 Kill
\end{code}
