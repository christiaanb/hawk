\begin{code}
module Processor where

import Hawk
import TransSig
import qualified Trans as T
import Components
import Utils
import DLX
\end{code}

\begin{code}
processor ((program,progmem),startPC) = bundleList [pc
                                                   ,writeback
                                                   ,pc'
                                                   ,pc' `bypassDst` writeback
                                                   ,instr
                                                   ,writeback'
                                                   --,bypassRegFile (instr,writeback')
                                                   ]
    where
    pc = delay (T.pcTrans startPC) (pc' `bypassDst` writeback)
    (instr,pc') = fetchDLX program pc
    writeback	= mem 4 progmem $ exec $ bypassRF instr writeback'
    writeback' = delay T.nop writeback -- $ trunc isTrapTrans writeback
\end{code}
