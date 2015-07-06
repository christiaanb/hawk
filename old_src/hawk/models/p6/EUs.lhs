<H3>EUs.hs for P6 Case Study</H3>

Execution Units

\begin{code}
module EUs where
import List
import qualified PreludeSig as Sig
import Hawk
import Trans
import DLX
\end{code}

\begin{code}
type EU i c = Signal Bool -> Signal [(Trans i c)] -> 
                Signal ([Trans i c],[Trans i c])
\end{code}

<!-- ------------------------------------------------- -->

Schedule Combinator

\begin{code}
schedule :: Register r => [EU i (c r w)] -> EU i (c r w)
schedule l b = foldl combine end (map ($b) l)
  where
  end = lift1 $ \x -> ([],x)
  combine f g sig = let (ac,rej) = unbundle2 $ f sig
                        (ac',rej') = unbundle2 $ g rej
                    in bundle2 (ac *++ ac',rej')
\end{code}

<!-- --------------------------------------------------------- -->

EUs

\begin{code}
addUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
addUnit = makeUnit isAdd aluDevice
\end{code}

\begin{code}
subUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
subUnit = makeUnit isSub aluDevice
\end{code}

\begin{code}
multUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
multUnit = makeDelayedUnit 4 isMul aluDevice
\end{code}

\begin{code}
divUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
divUnit = makeDelayedUnit 8 isDiv aluDevice
\end{code}

\begin{code}
cmpUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
cmpUnit = makeUnit isCmp aluDevice
\end{code}

\begin{code}
jumpUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
jumpUnit = makeUnit isJump (map jumpDevice)
\end{code}

\begin{code}
moveUnit :: (Cell c,Register r,Word w,Instruction i) => EU i (c r w)
moveUnit = makeUnit isMove aluDevice
\end{code}

<!-- ------------------------------------------------------------- -->

Devices

\begin{code}
jumpDevice trans@(Trans [dest] op [cond,src1,src2] _) | isJumpOp op =
        (trans `evalTrans` 
            (dest,alu func val1 val2))
   where func = if (getVal cond) == 0 then z else nz
         val1 = getVal src1
         val2 = getVal src2
         z = fstOp op
         nz = sndOp op
\end{code}

\begin{code}
aluDevice x 
     = map (\t@(Trans [dest] op [src1,src2] x) -> 
              let aluFunc = aluOp op
              in (t `evalTrans` (dest,
                    (alu aluFunc (getVal src1) (getVal src2))))
           ) x
\end{code}

<!-- -------------------------------------------- -->

Higher-order Constructors

\begin{code}
makeUnit :: Register r => (Trans i (c r w) -> Bool) -> 
                          ([Trans i (c r w)] -> [Trans i (c r w)]) -> 
                          EU i (c r w)
makeUnit accept unit kill instrs
   = lift1 (\(k,x) -> let (acceptable,rejects) = partition accept x
                          (instr,others) = splitAt 1 acceptable
                   in (unit instr,others ++ rejects)) $ bundle2 (kill,instrs)
\end{code}


\begin{code}
makeDelayedUnit :: Register r => Int -> (Trans i (c r w) -> Bool) -> 
                                 ([Trans i (c r w)] -> [Trans i (c r w)]) -> 
                                 EU i (c r w)
makeDelayedUnit n f g kill sig = bundle2 (flush n [] kill $ delayN n [] x,y)
  where
  (x,y) = unbundle2 $ unit kill sig
  unit = makeUnit f g
\end{code}

\begin{code}
delayN n x s = xs `before` s
   where xs = take n (repeat x)
\end{code}
