-- Execution units --

module OA64_EU where
import List
import Ix

import qualified PreludeSig as Sig
import Hawk
import Trans

import OA64

import OA64_Utils

type EU i c = (Signal Bool,Signal [(Trans i c)])  -> 
              (Signal [Trans i c],Signal [Trans i c])

type Stalling_EU i c = (Signal Bool,Signal [(Trans i c)])  -> 
                    (Signal Bool,Signal [Trans i c],Signal [Trans i c])

-- Schedule Combinator ---------------------------------------------------
mk_alu :: [EU i (c r w)] -> EU i (c r w)
mk_alu l (b,s) = foldl combine end (map (\f s -> f(b,s)) l) s
  where
  end = unbundle2 . (lift1 $ \x -> ([],x))
  combine f g sig = let (ac,rej) = f sig
                        (ac',rej') =  g rej
                    in (ac *++ ac',rej')

-- EUs -----------------------------------------------------------
cmp_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
cmp_unit k = pipe_unit k isCmp aluDevice

bool_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
bool_unit k = pipe_unit k isBool aluDevice

add_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
add_unit k = pipe_unit k isAdd aluDevice

sub_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
sub_unit k = pipe_unit k isSub aluDevice

mlt_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
mlt_unit k = pipe_unit k isMul aluDevice

div_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
div_unit k = pipe_unit k isDiv aluDevice

jmp_unit :: (Cell c,Register r,Word w,Instruction i) => Int -> EU i (c r w)
jmp_unit k = pipe_unit k isJump aluDevice 

fence_unit k = pipe_unit k isFence id

type ExTable i w = [(i,w)]

chk_unit :: (Register r,Word w) => 
            Int ->  (ExTable OA64_Op w,ArrayDesc w w) -> 
            EU OA64_Op (OA64_Cell r w)
chk_unit k (table,m) = 
   pipe_unit k isCheck (map check)
   where 
   bounds = fst m
   check t@(Trans d@(dest:_) op s@(src:_) i) | isCheckOp op
      = if isAcid src then 
              let ick = Trans (putVal pcNothing err:d) op s i
              in ick `evalTrans` (dest,Nothing)
        else t `evalTrans` (dest,Just $ getVal src)
   check t = t
   err = lookup (ERR LOAD) table

cache k (table,m) input = stalls `before` ss_mem 1 m (fmap (map check) input)
   where 
   stalls = take k (repeat [])
   bounds = fst m
   check t@(Trans d@(dest:_) LOAD_S s@(address:offset:_) i) 
        = if inRange bounds (getVal address + getVal offset) then t
          else Trans (map sizzle d) (ERR LOAD_S) s i
   check t@(Trans d LOAD s@(address:offset:_) i) 
        = if inRange bounds (getVal address + getVal offset) then t
          else let op = ERR LOAD
                   ick = Trans (putVal pcNothing (lookup op table):d) op s i
               in ick `evalTrans` (head d,Nothing)
   check t@(Trans d STORE s@(address:offset:_) i) 
        = if inRange bounds (getVal address + getVal offset) then t
          else let op = ERR STORE
               in Trans (putVal pcNothing (lookup op table):d) op s i
   check t = t




aluDevice x = map trans_alu x


-- Higher-order Constructors   ----------------------------------------------

comb_unit :: (Trans i c -> Bool) -> ([Trans i c] -> [Trans i c]) -> EU i c 
comb_unit accept unit (kill,instrs)
   = unbundle2 $ 
     lift1 (\(k,x) -> let (acceptable,rejects) = partition accept x
                          (instr,others) = splitAt 1 acceptable
                      in (unit instr,others ++ rejects)) 
     $ bundle2 (kill,instrs)


------------------------------------------------------------------------

pipe_unit :: Int -> (Trans i c -> Bool) -> ([Trans i c] -> [Trans i c]) -> 
             EU i c
pipe_unit n f g (kill,sig) = (flush n [] kill $ delayN n [] x,y)
  where (x,y) = comb_unit f g (kill,sig)

------------------------------------------------------------------------

stall_unit :: EU i c -> Stalling_EU i c
stall_unit unit (kill,sig) = (s,x,y)
  where (x,y) = unit (kill,sig)
        sig' = if' s then' (lift0 []) else' sig
        s = del $ if' s
                   then' (Sig.length x *<= s0) 
                   else' (Sig.length sig *> s0 *&& Sig.length x *<= s0)


------------------------------


delayN n x s = xs `before` s
   where xs = take n (repeat x)
