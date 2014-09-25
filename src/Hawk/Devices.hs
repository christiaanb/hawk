module Hawk.Devices where

import CLaSH.Signal        (Signal, bundle')
import Control.Monad       (when)
import Control.Monad.State (get,put)
import Hawk.Signal

-- | @'flush' k x b s@ return @x@ for @k@ cycles when @b@ is true, otherwise it
-- returns s.
--
-- >>> sample (flush 2 10 (fromList [False,False,True,False,False,False,False]) (fromList [1,2,3,4,5,6,7]))
-- [1,2,10,10,5,6,..
flush :: Int -> a -> Signal Bool -> Signal a -> Signal a
flush n d s1 s2 = loop 0 (bundle' (s1,s2)) $ \(s1,s2) -> do
  when s1 (put n)
  n <- get
  if n > 0
     then do put (n-1)
             return d
     else return s2
