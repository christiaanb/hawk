module Hawk.Signal where

import CLaSH.Prelude
import Control.Monad.State (State, runState)

loop :: s -> Signal a -> (a -> State s b) -> Signal b
loop iS a f = o
  where
    (o,s') = unbundle ((\a s -> runState (f a) s) <$> a <*> s)
    s      = register iS s'
