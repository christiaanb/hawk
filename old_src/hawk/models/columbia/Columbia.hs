module Columbia where

import LazyST

import Hawk
import TransSig
import qualified Trans as T
import qualified PreludeSig as P

import OA64

import OA64_Utils
import OA64_IFU
import OA64_Cluster
import OA64_EU
import OA64_Queue
import OA64_RF
import OA64_Sift

packet_size = 4 :: Int
packet_size' = lift0 packet_size


columbia ((p,p_mem),start) = probe "retired" retired
  where
  fence' = delay False fence
  next_loc' = delay (T.pcTrans start) next_loc
  fetch' = delay False fetch

  (fetched,next_loc) = ifu (packet_size,p) (loc) (k)
  loc = if' valid then' new_loc else' next_loc'



  (fetch,fence) = logic (contains_fence fetched,flushed ,space)
  space = num *>= packet_size'

  k =  if' fetch' then' packet_size' else' s0

  (num,flushed,retired,new_loc,valid) = core 4 p_mem (fetched,delay False fence)

rmFences s = lift1 (filter (not . isFence)) s

valid_logic (f,retired) = f *&& valid
   where
   valid = if' ((delay False f) *&& (lift1 not f)) then' (lift0 False)
           else' ((contains_valid_jump retired) *|| (delay False valid))
   

core n pgm_mem (instrs,f) = (a,d,retired,new_pc,valid)
   where
{-
   valid = if' ((delay False f) *&& (lift1 not f)) then' (lift0 False)
           else' ((contains_valid_jump retired) *|| (delay False valid))
-}
   valid = valid_logic (f,retired) 

   d = foldl1 (*&&) ds
   a = P.minimum (bundleList as)
   c = lift1 concat (bundleList cs)
   instrs' = sift_trans n (rmFences instrs)

   (ds,as,cs,dqs,mis)
         = clusters n (packet_size,exec_unit pgm_mem) (instrs',rs,mos)
   (rs,retired,new_pc) = multi_rf (dqs,c)
   mos         = multi_cache 1 (e_table,pgm_mem) mis
   err           = lift0 False



clusters 0 cfg (_,_,_) = ([],[],[],[],[])
clusters n cfg ~(i:is,r:rs,mo:mos) = 
   let ~(d,a,c,dq,mi) = cluster cfg (lift0 False,i,r,mo)
       ~(ds,as,cs,dqs,mis) = clusters (n-1) cfg (is,rs,mos)
   in (d:ds,a:as,c:cs,dq:dqs,mi:mis)


exec_unit pgm_mem 
    = mk_alu [add_unit 0
             ,sub_unit 0
             ,jmp_unit 0
             ,mlt_unit 4
             ,div_unit 8
             ,cmp_unit 0 
             ,bool_unit 0
             ,fence_unit 0
             ,chk_unit 0 (e_table,pgm_mem)
             ]

e_table :: [(OA64_Op, Int)]
e_table = [(ERR LOAD,256),(ERR STORE,257)]

--logic (fencing,flushed,gt_k) = (fetch,fence)
--  where fence = delay False $ (fence *&& (P.not flushed)) *|| fencing
--        fetch = (P.not fence) *&& gt_k
logic (fencing,flushed,gt_k) = (fetch,fence)
  where fence = ((delay False fence) *&& (P.not flushed)) *|| fencing
        fetch = (P.not fence) *&& gt_k


contains_fence instrs = lift1 (\x -> length (filter isFence x) > 0) instrs
contains_valid_jump instrs 
   = lift1 (\x -> length (filter (\x -> T.isBranch x && T.evalPredicate x > 0) x) > 0) 
     instrs


-----------------------------------------------------------------
min_circ x = P.minimum (bundleList x)
concat_circ x = lift1 concat (bundleList x)
and_circ x = foldl (*&&) (lift0 True) x

demo_logic (fetched,flushed,num) = (k,fence)
   where
   k = if' fetch then' packet_size' else' s0
   (fetch,fence) = logic (contains_fence fetched,flushed ,space)
   space = num *>= packet_size'
