{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module Hawk.Trans where

import Data.Default       (Default (..))
import CLaSH.Sized.Vector (Vec,(++))
import GHC.TypeLits       (KnownNat, type (+))
import Prelude            hiding ((++))

data Trans i c = Trans (Vec 2 c) i (Vec 3 c) (Vec 1 c)
  deriving (Eq,Show)

fill :: (Default a,KnownNat m)
     => Vec n a
     -> Vec (n + m) a
fill xs = xs ++ def

trans d o a i = Trans (fill d) o (fill a) (fill i)
