<H3>Prediction Unit for Pipelined Speculative Processor Example</H3>

<!-- \outline{\section*{Prediction Unit}} -->

\begin{code}
module PipePredict 
  (
      btb,btbSS 
     ,annotate
     ,mispredicted
  ) where

import Hawk
import LazyST
import qualified Trans
import TransSig
import DLX
import PipeUtils
\end{code}

<HPPTAG getstr tt_on>branchTargetBuffer<HPPTAG getstr tt_off>, 
based on incoming jump instructions <HPPTAG getstr tt_on>instrs<HPPTAG getstr tt_off> predicts what
the next PC will be.   It uses 
<HPPTAG getstr tt_on>feedback<HPPTAG getstr tt_off> to build its table.

\begin{code}
btbSS size start instrs feedback
  = fmap (map Trans.pcTrans) $
    runST ( do cache <- newSTArray (0,size) Nothing
               loop (bundle2 (feedback,instrs)) $
                 \(f,i) -> do {mapM (update size cache) f; mapM (predict size start cache) i}
          )
\end{code}

\begin{code}
btb size start instrs feedback 
  = pcTrans $ 
    runST ( do cache <- newSTArray (0,size) Nothing
               loop (bundle2 (feedback,instrs)) $
                 \(f,i) -> do {update size cache f; predict size start cache i}
          )
--  where 
\end{code}
<HPPTAG getstr tt_on>update<HPPTAG getstr tt_off> (which is written in the state and exceptions monad) gets the location and
updated PC and updates the buffer.  If anything goes wrong, the instruction probably isn't
a branch instruction, so just return <HPPTAG getstr tt_on>()<HPPTAG getstr tt_off>

\begin{code}
update size cache t 
   = do Loc loc <- getLocation' t
        Reg PC (Val pc)  <- getDestPC' t
        writeArraySTEx cache (loc `mod` size) (Just pc)
     `handle` ()
\end{code}

<HPPTAG getstr tt_off>predict<HPPTAG getstr tt_off> gets the location and returns the pc in the buffer.  If anything
goes wrong (for example, if the buffer does not contain a PC), then just return the
default <HPPTAG getstr tt_off>start<HPPTAG getstr tt_off>

\begin{code}
predict size start cache t 
   = do Loc loc <- getLocation' t
        x <- readArraySTEx cache (loc `mod` size)
        return $ x `catchEx` start
     `handle` start
\end{code}

<HPPTAG getstr tt_on>annotate<HPPTAG getstr tt_off> 
places predicted target into the information field of the
branch transaction

\begin{code}
annotate :: Word a => Signal (DLX_Trans a) -> Signal (DLX_Trans a) -> Signal (DLX_Trans a)
annotate = lift2 $ \t -> do Reg PC (Val x) <- Trans.getDstPC t
                            return (\t -> Trans.putInfo t [Reg SpecPC (Val x)])
                  `catchEx` id        
\end{code}
 

<HPPTAG getstr tt_on>mispredicted<HPPTAG getstr tt_off> returns 
<HPPTAG getstr tt_on>True<HPPTAG getstr tt_off> if the instruction is a branch
and the target was mispredicted

\begin{code}
mispredicted :: Word a => Signal (DLX_Trans a) -> Signal Bool
mispredicted = lift1 $ \t -> do Reg SpecPC (Val sp)   <- Trans.getSpecPC t
                                Reg PC (Val pc) <- Trans.getDstPC t
                                return (sp /= pc)
                      `catchEx` False
\end{code}
