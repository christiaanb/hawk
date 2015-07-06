module OA64_Demo where

import IO
import Hawk
import OA64
import OA64_Cluster
import OA64_IFU
import OA64_Sift
import Trans
import Columbia

oa64_p1 :: ((OA64_InstrMem,OA64_DataMem),OA64_Word)
oa64_p1 = assemble program 

{-
program =
     [
        oa_addi R2 R0 7      P1 2          -- 1
       ,oa_addi R1 R0 1      P1 1      
       ,oa_addi P2 R0 1      P1 1      
       ,oa_fence 0
       ,oa_ne2 P2 P3 R2 R0   P1 0          -- 5
       ,oa_addi R3 R2 0      P1 1
       ,oa_nop
       ,oa_fence 0                        
       ,oa_mult R1 R1 R3     P2 1          -- 9
       ,oa_subi R2 R2 1      P2 2         
       ,oa_j 5               P2 3 
       ,oa_fence 0          
       ,oa_addi R3 R1 0      P3 3          -- 13
       ,oa_j 13              P3 4         
       ,oa_nop
       ,oa_fence 0
       ]
-}
program =
     [
        oa_addi R2 R0 7      P1 2          -- 1
       ,oa_addi R1 R0 1      P1 1      
       ,oa_addi P2 R0 1      P1 1      
       ,oa_fence 0
       ,oa_ne2 P2 P3 R2 R0   P1 0          -- 5
       ,oa_addi R3 R2 0      P1 1
       ,oa_nop
       ,oa_fence 0                        
       ,oa_mult R1 R1 R3     P2 1          -- 9
       ,oa_subi R2 R2 1      P2 2         
       ,oa_j 5               P2 3 
       ,oa_nop
       ,oa_addi R3 R1 0      P3 0          -- 13
       ,oa_j 9              P3 1         
       ,oa_nop
       ,oa_fence 0
       ]
{-
program =
     [
        oa_addi R2 R0 7      P1 0         -- 1
       ,oa_addi R1 R0 1      P1 1      
       ,oa_addi P2 R0 1      P1 2

       ,oa_fence 0                        -- 4
       ,oa_fence 1
       ,oa_fence 2

       ,oa_mult R1 R1 R2     P2 1      
       ,oa_ne2 P2 P3 R2 R0  P1 0         
       ,oa_subi R2 R2 1      P2 2         

       ,oa_j 4               P2 3         
       ,oa_nop
       ,oa_nop                            

       ,oa_nop                            -- 13
       ,oa_addi R3 R1 0      P3 0        
       ,oa_j 13              P3 1         

       ]

-}

gen_alu :: (Signal Bool,Signal [OA64_Trans]) -> 
          (Signal [OA64_Trans],Signal [OA64_Trans])
gen_alu = exec_unit (snd $ fst $ assemble []) 

cluster5 :: Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)] -> 
            Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)] -> 
            Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)] -> 
            (Signal Bool,Signal Int
            ,Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)] 
            ,Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)]
            ,Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)]
            )

cluster5 x y z = cluster (5,gen_alu) (no_err,x,y,z)

multi_cache3 :: Register a => Signal [Trans OA64_Op (OA64_Cell a Int)] -> 
                              Signal [Trans OA64_Op (OA64_Cell a Int)] -> 
                              Signal [Trans OA64_Op (OA64_Cell a Int)] -> 
                             (Signal [Trans OA64_Op (OA64_Cell a Int)]
                             ,Signal [Trans OA64_Op (OA64_Cell a Int)]
                             ,Signal [Trans OA64_Op (OA64_Cell a Int)]
                             )

multi_cache3 x y z = let [x',y',z'] = multi_cache 1 (e_table,snd $ fst $ oa64_p1) [x,y,z]
                     in (z',y',x')

----ACKK ! LOOK, RECALL, REMEMBER!  xyz is reversed in result
{-
multi_rf3 :: (Show a, Word b, Register c) => 
    Signal [Trans a (OA64_Cell c b)] -> 
    Signal [Trans a (OA64_Cell c b)] -> 
    Signal [Trans a (OA64_Cell c b)] -> 
    Signal [Trans a (OA64_Cell c b)] -> 
   (Signal [Trans a (OA64_Cell c b)]
   ,Signal [Trans a (OA64_Cell c b)]
   ,Signal [Trans a (OA64_Cell c b)]
   ,Signal [Trans a (OA64_Cell c b)]
   ,Signal (Trans a (OA64_Cell c b))
   )
-}

multi_rf3 x y z wb = let ([x',y',z'],r,pc) = multi_rf ([x,y,z],wb)
                     in (z',y',x',r,pc)


valid_log x y = valid_logic (x,y)
fetch_logic x y z  = demo_logic(x,y,z)
and_circ3 x y z = and_circ [x,y,z]
min_circ3 x y z = min_circ [x,y,z]
concat_circ3 x y z = concat_circ [x,y,z]

no_err = lift0 False

p1_ifu :: Signal (Trans OA64_Op (OA64_Cell OA64_Reg Int)) -> 
          Signal Int ->  
         (Signal [Trans OA64_Op (OA64_Cell OA64_Reg Int)]
         ,Signal (Trans OA64_Op (OA64_Cell OA64_Reg Int))
         )

p1_ifu = ifu (5,fst $ fst $ oa64_p1)

{-
sift_instrs3 :: Signal [Trans a (OA64_Cell b c)] -> 
               (Signal [Trans a (OA64_Cell b c)]
               ,Signal [Trans a (OA64_Cell b c)]
               ,Signal [Trans a (OA64_Cell b c)]
               )
-}

sift_instrs3 i = let [x,y,z] = sift_trans 3 (rmFences i)
                 in (x,y,z)

instance Init OA64_Trans where
   def = pcTrans 1

defence s = rmFences s
  

