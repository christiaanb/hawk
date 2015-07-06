module OA64_RF(rf,RF) where

import Prelude hiding (read)
import Ix
import LazyST
import IOExts

import Trans
import Hawk

import OA64

import OA64_Utils


type RF i c r w = (Signal [Trans i (c r w)],Signal [Trans i (c r w)]) 
        -> (Signal [Trans i (c r w)],Signal [Trans i (c r w)])

rf :: (Register r, Word w, Show i) => RF i OA64_Cell r w
rf (reads,writes) = unbundle2 $ runST (
  do { file <- new
--     ; step2(reads,writes) 
     ; loop(bundle2(reads,writes)) $ \ ~(reads,writes) -> do 
                           { writes' <- mapM (exec_writes file) writes
                           ; reads' <- mapM (exec_reads  file) reads
                           ; return (reads',writes')
                           }
     }
  )


exec_writes f t@(Trans dst _ _ _) = 
 do { t' <- if isPredicated t  && not (readOnly $ getReg $ getPredicate t) then
               do { let p = getPredicate t
                  ; let pr = getReg p
                  ; prv <- read f pr
                  ; let t' = case prv of
                                 Nothing -> fillInSrcCells' t [sizzle p]
                                 _ -> fillInSrcCells' t [putVal p prv]
                  ; return t'
                  }
           else return t
    ; let p = evalPredicate t'
    ; mapM (\c ->  inCase (isReg c) $
              do { let i = getReg c
                 ; let bounds = boundsSTArray f
                 ; inCase (inRange bounds i && not (readOnly i) && p /= 0) $
                        if isAcid c then write f i Nothing
                        else write f i (Just $ getVal c)
                 }
           ) dst
    ; return t'
    }

exec_reads f (Trans dst op src info) = 
     do { src' <- mapM ( \c -> 
                          if isReg c then 
                           do { let i = getReg c
                              ; let bounds = boundsSTArray f
                              ; v <-  if inRange bounds i 
                                       then read f $ getReg c
                                       else return $ error "reg out of range"
                              ; return $ if not (isAss c) then 
                                               case v of
                                                  Nothing -> sizzle c
                                                  _ -> putVal c v
                                         else c
                              }
                          else return c
                       ) src
        ; return $ Trans dst op src' info
        }

type RF_ST s a b = STArray s a (Maybe b)

new   :: (Register a,Num b) => ST c (RF_ST c a b)
read  :: Register a => RF_ST s a b -> a -> ST s (Maybe b)
write :: Register a => RF_ST s a b -> a -> (Maybe b) -> ST s ()

new         = newSTArray (minBound,maxBound) (Just 0)
read        = readSTArray 
write f x z = inCase (not $ readOnly x) $ writeSTArray f x z
