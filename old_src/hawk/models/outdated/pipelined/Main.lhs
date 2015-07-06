\begin{code}

module Main where

import System(getArgs)
import Hawk
import Pipeline(pipeline)
import Signal

import DLX
import DLX_Cell
import DLX_Reg
import DLX_Op
import Trans

main =
 do args <- getArgs
    case args of
        [file]    -> hawkMain file (mapM_ (putStrLn . show) . (view . p))
        otherwise    -> error "usage: prog filename\n"
    return ()


p :: ((ArrayDesc Int (Instr DLXReg Int),((Int,Int),[(Int,Int,Int)])),Int) -> Signal (Trans DLX_Op (DLX_Cell DLXReg Int))
p = pipeline



\end{code}
