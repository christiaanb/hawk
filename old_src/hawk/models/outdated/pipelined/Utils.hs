module Utils where

import Hawk
import Trans
import qualified TransSig as T
import DLX
import Maybe


isExecTrans trans
  = lift1 not (modifiesPC trans) *&& map isExec trans
    where
      isExec (Trans _ op _ _)
        = case op of
                (NoOp _ )     -> False
                _               -> True


modifiesPC trans
  = lift1 not $ map null $ T.filterDst isPC trans


mkTrans x = map (\x -> nop) x 


isTrapTrans :: Signal DLXTrans  -> Signal Bool
isTrapTrans = lift1 isTrap
   where isTrap (Trans [ _ , Reg IAR _ ] _ _ _ )    = True
         isTrap _                                              = False

branchTaken trans
  = lift1 not $ (lift1 null) $ T.filterDst (\c -> isPC c && isAss c) trans
 
 
