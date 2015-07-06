module OA64_IFU where


import LazyST
import Ix

import Hawk
import Trans
import qualified PreludeSig as Signaled

import Word


ifu (a,b) c d = (rmNops . unique',id) >< fetch (1,id,a,b) c d
  where rmNops s = fmap (filter (not . isNop)) s


unique' ts = runST (
  do { v <- newSTRef 1
  --   ; step1(ts) 
     ; loop(ts) $ \ ts -> do
                 { mapM (\(Trans x y z i)  -> do { v' <- readSTRef v
                                  ; writeSTRef v (v'+1)
                                  ; return $ Trans x y z (i ++ [loc v'])
                                  }) ts
                 }
     }
  )
