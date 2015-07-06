<H3>Memory.lhs</H3>
\begin{code}
module Memory where

import Word
import Ix
import Maybe
import Arithmetic
import Array
import Words
\end{code}

-- Begin Signature -------------------------------------------------
<P>

   Some types to describe encodings of memory state and the 
   communication to memory


\begin{code}
type ArrayDesc i v = ((i,i),[(i,i,v)])
\end{code}

\begin{code}
type InstrMemoryState w i = ArrayDesc w i
\end{code}

\begin{code}
type MemoryState w i = (InstrMemoryState w i,DataMemoryState w)
\end{code}

\begin{code}
type DataMemoryState w = ArrayDesc w w
\end{code}

\begin{code}
data WordSize = Byte | HalfWord | FullWord
          deriving (Eq,Show, Read)
\end{code}

\begin{code}
data LoadStoreOp = Load WordSize Sign
                 | Store WordSize 
                 | NOP  -- No operation
           deriving (Eq,Show, Read)
\end{code}



Array request
\begin{code}
data ArrReq i a  = ReadArr i |
		   WriteArr i i a |
		   WriteFn i (a -> a) |	-- modify contents at location i
		   FreezeArr
		   --deriving Show
\end{code}

Array response
\begin{code}
data ArrResp i a = ReadVal a |
		   Written |
		   WrittenFn a |
		   ArrayVal (Array i a)
		   deriving Show
\end{code}

-- End Signature -------------------------------------------------------


