\begin{code}

module Main where

import System(getArgs)
import Hawk
import Processor(processor)
import Signal

import DLX
import DLX_Cell
import DLX_Reg
import DLX_Op
import Trans
\end{code}

\begin{code}
main =
 do args <- getArgs
    case args of
        [file]    -> run file 
        otherwise    -> error "usage: prog filename\n"
    return ()
\end{code}

\begin{code}
run file =  hawkMain file (mapM_ (putStrLn . show) . (view . p))
\end{code}

\begin{code}
p :: ((ArrayDesc Int (Instr DLXReg Int),((Int,Int),[(Int,Int,Int)])),Int) 
        -> Signal [Trans DLX_Op (DLX_Cell DLXReg Int)]
p = processor
\end{code}
