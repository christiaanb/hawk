module Hawk.Signal where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (State, runState)
import CLaSH.Signal        (Signal, register, unbundle)

loop :: s -> Signal a -> (a -> State s b) -> Signal b
loop iS a f = o
  where
    (o,s') = unbundle ((\a s -> runState (f a) s) <$> a <*> s)
    s      = register iS s'
