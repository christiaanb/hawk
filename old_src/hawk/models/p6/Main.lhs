<H3>Main.lhs for P6 Case Study</H3>
Module header:

\begin{code}
module Main where
\end{code}

Import Hawk library, Processor (in this case the p6), LazyST monad.
\begin{code}
import System
import Hawk--(hawkMain)
import DLX
import DLX_Cell
import DLX_Reg
import DLX_Op
import Processor
import Signal
import Trans
import LazyST
import Probe
\end{code}
<P>
The main program is written using the ST monad.  It can be
considered as a sequential program.

\begin{code}
main =
 do { --clearProbes_MS
      clearProbes_UNIX
    ; args <- getArgs
    ; case args of
        [file]           -> run file
        ["-count", file] -> count file
        ["-sample",n,file] -> sample (read n) file
        otherwise        -> error "Usage: pgm [-count] file\n"
    ; return ()
    }
\end{code}

\begin{code}
p :: ((ArrayDesc Int (Instr DLXReg Int), a), Int) -> Signal [Trans DLX_Op (DLX_Cell DLXReg Int)]
--p :: ((ArrayDesc Int (Instr DLXReg Int), a), Int) -> Signal [Trans DLX_Op (DLX_Cell (Virtual DLXReg Int) Int)]
p = processor
\end{code}

\begin{code}
run file =
     hawkMain file (mapM_ (putStrLn . show) . (view . p))
\end{code}

\begin{code}
count file =
     hawkMain file (mapM_ (putStrLn . show) . (cnt. view . p))
\end{code}

\begin{code}
sample n file =
     hawkMain file (mapM_ (putStrLn . pretty) . (take n . view . p))
\end{code}

\begin{code}
pretty tss =
    foldr (\x y -> show x ++ "\n" ++ y) "\n" tss
\end{code}

\begin{code}
cnt l = runST (
  do n <- newSTRef 0
     mapM (\l -> n+=(length l)) l
  )
\end{code}

\begin{code}
n += v =
  do n' <- readSTRef n
     let v' = v+n'
     writeSTRef n v'
     return v'
\end{code}
