module OA64_Utils where
import Maybe

import Hawk
import qualified Trans
import qualified PreludeSig as S
import TransSig

import OA64

s0 :: Signal Int
s0 = lift0 0
s1 :: Signal Int
s1 = lift0 1
s2 :: Signal Int
s2 = lift0 2
s3 :: Signal Int
s3 = lift0 3
s4 :: Signal Int
s4 = lift0 4
s5 :: Signal Int
s5 = lift0 5
s6 :: Signal Int
s6 = lift0 6
s7 :: Signal Int
s7 = lift0 7

singleton = lift1 $ \x -> [x]

true = lift0 True
false = lift0 False

no = lift1 not

nothing = lift0 Nothing



inCase True x = x
inCase False x = return ()
