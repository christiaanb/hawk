\begin{code}
module Probe where
import IO
import Arithmetic
import Cell
import Devices
import Memory
import Trans
import VRegister
import Word
import Signal
import PipeReg
import Utilities

import Trans

import Prettier

import IOExts

import System
\end{code}

-- Begin Signature -------------------------------------------------------

{- 
   In practice it is nice to be able to place a probe on a signal.
   Suppose that "s" is a signal.  'probe "s.output" s' has the
   same meaning as 's' --- but s's contents have been written to 
   the file "s.output".   This function has turned out to be
   critical in the development of the Visio Hawk interface.

   Some issues to consider:
        * probe is a hack --- and it messes with referential 
          trancparency.  Some Haskell compilers may wreak havoc with
          probes.  
	* probe is pretty careful not to change the strictness
          behavior.
        * Avoid using probes within unit definitions.  If you duplicate
          the use of the unit, the output file will be written to 
          simultaniously by both units instantiations.  Try using probe
          only at the top-level of your microarchitecture.
        * Probes are typically stored in a subdirectory "Probes/"
-}
   

\begin{code}
class Show a => Probe a where
  probe :: String -> Signal a -> Signal a
  ppout :: a -> Doc
  outp :: a -> String
  probe n (List vals) = List (zipWith (dataOut n) [1..] vals)
  ppout = text . outp
  outp = show
\end{code}

-- tries to do the right thing in the case of UNIX or Microsoft Windows
\begin{code}
clearProbes :: IO()
\end{code}

-- clear the probes subdirectory in UNIX
\begin{code}
clearProbes_UNIX :: IO ()
\end{code}

-- clear the probes subdirectory in Microsoft
\begin{code}
clearProbes_MS :: IO ()
\end{code}

\begin{code}
instance (Show a,Show b) => Probe (a,b)
instance (Show a,Show b,Show c) => Probe (a,b,c)
instance Probe Bool
instance Probe Int
instance Probe Word32
instance Probe Word64
instance Probe Char
instance Probe a => Probe (Maybe a)
{-instance (Probe a, Probe b) => Probe (Virtual a b )-}
{-instance Probe PipeRegCmd-}
{-instance Probe AluOp-}
\end{code}


-- End Signature --------------------------------------------------------
\begin{code}
clearProbes 
   = do { w <- windows
        ; if w then clearProbes_MS
          else clearProbes_UNIX
        }
\end{code}

\begin{code}
clearProbes_UNIX
--   = do { system "rm -f Probes/* >& /dev/null"
   = do { putStr "removing "
        ; system "echo Probes/*"
        ; system "rm Probes/*"
        ; return ()
        }
\end{code}

\begin{code}
clearProbes_MS
   = do { system "del /Q Probes\\*.*"
        ; return ()
        }
\end{code}

\begin{code}
dataOut :: Probe a => String -> Int -> a -> a
dataOut fileName clock val = unsafePerformIO $
  do
    {h <- openFile ("Probes/" ++ fileName) AppendMode;
    hPutStrLn h
      (pretty 78 (text (show clock ++ ":") <:> indent 5 <:> tab (ppout val)));
    hClose h;
    return val}
\end{code}



\begin{code}
instance (Probe a, Probe b) => Probe (Virtual a b ) where
   outp (Virtual n (Just r)) = "V"++show n ++ "{" ++ outp r ++"}"
   outp (Virtual n Nothing) = "V"++show n
   outp (Real r) = outp r
\end{code}


\begin{code}
instance Probe PipeRegCmd where
   outp Input = "Ok"
   outp Kill  = "Kill"
   outp Stall = "Stall"
\end{code}

\begin{code}
instance Probe AluOp where
  outp (Add _) = "+"
  outp (Sub _) = "-"
  outp (Div _) = "/"
  outp (Mult _) = "*"
  outp And  = "AND"
  outp Or  = "OR"
  outp Xor  = "XOR"
  outp Not  = "NOT"
  outp Input1 = "fst"
  outp Input2 = "snd"
  outp x = show x
\end{code}



\begin{code}
instance Probe a => Probe [a] where
   ppout xs = ppsep ppout line xs
\end{code}
