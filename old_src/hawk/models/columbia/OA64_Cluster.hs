module OA64_Cluster where
import Maybe

import Hawk
import TransSig
import qualified Trans
import qualified PreludeSig as S

import OA64

import OA64_Utils
import OA64_Queue
import OA64_EU
import OA64_RF
import OA64_Sift

-------------------------------------------------------------------
-- Multiple ported Shared Register File ---------------------------

--multi_rf :: (Register a, Word b, Cell c) => 
--            ([Signal [Trans d (c a b)]],[Signal [Trans d (c a b)]]) -> 
--            ([Signal [Trans d (c a b)]],Signal [Trans d (c a b)])
multi_rf (reads,writes) = (unbundleList outputs,n_writes,fliparound pc)
   where outputs = lift2 chop lengths output
         reads' = bundleList reads
         lengths = fmap (map length) reads'
         (houtput,n_writes) = rf (concat' pc_r reads',writes)
         pc = lift1 head houtput
         output = lift1 tail houtput
         pc_r = lift0 $ Trans [] NOOP [Reg PC Q] []
 
         fliparound = lift1 $ \(Trans x op y i) -> Trans y op x i

         concat' op reads = lift2 (:) op (lift1 concat reads)

         chop [n] xs = [xs]
         chop (n:ns) xs = y:chop ns ys
              where (y,ys) = splitAt n xs
         chop _ _ = []

multi_cache k cfg l = sift_trans n (cache k cfg ls)
   where n = length l
         ls = foldl (*++) (lift0 []) l

-------------------------------------------------------------------
-- Cluster = Queue + stalling EU


type OA64_RF = RF OA64_Op OA64_Cell OA64_Reg OA64_Word
type OA64_Cluster = Cluster OA64_Trans 
type OA64_EU = EU OA64_Op (OA64_Cell OA64_Reg OA64_Word)
type OA64_EUSched =  [OA64_EU] -> OA64_EU



type Cluster t = (Signal Bool,Signal [t],Signal [t],Signal [t]) -> 
                 (Signal Bool,Signal Int,Signal [t],Signal [t],Signal [t])

cluster :: (Int,OA64_EU) -> OA64_Cluster
cluster (n,alu) (err,inputs,from_rf,mem_vals) 
   = (empty,space_avail,outputs,to_rf,mem_ops)
  where

  (space_avail,to_rf) = queue n (err,inputs,release1)
  release1 =  if' busy then' s0 else' s1
  (mem_ops,arith_ops) = S.partition Trans.isMem $ from_rf

  (alu_busy,computed,_) = stall_unit alu (err,arith_ops)

  busy = alu_busy *|| cache_busy(mem_ops,mem_vals)

  outputs = del (computed *++ mem_vals)
  empty =   space_avail *== lift0 n 
        *&& no busy  
        *&& S.length from_rf *== s0


cache_busy (inputs,outputs) = s
   where 
   s = if' (del s) then' (S.length outputs *> s0)
                   else' (S.length (del inputs) *> s0)
