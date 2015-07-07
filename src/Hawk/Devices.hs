{-# LANGUAGE DataKinds, NoImplicitPrelude, TypeOperators #-}
module Hawk.Devices where

import CLaSH.Prelude
import Control.Monad       (guard,when)
import Control.Monad.State (get,put)
import Hawk.Cell
import Hawk.Instruction
import Hawk.Register
import Hawk.Signal
import Hawk.Trans
import Hawk.Utilities

-- | @'flush' k x b s@ return @x@ for @k@ cycles when @b@ is true, otherwise it
-- returns s.
--
-- >>> sample (flush 2 10 (fromList [False,False,True,False,False,False,False]) (fromList [1,2,3,4,5,6,7]))
-- [1,2,10,10,5,6,..
flush :: Int -> a -> Signal Bool -> Signal a -> Signal a
flush n d s1 s2 = loop 0 (bundle (s1,s2)) $ \(s1,s2) -> do
  when s1 (put n)
  n <- get
  if n > 0
     then do put (n-1)
             return d
     else return s2

regFile :: (Enum i, KnownNat n)
        => Vec n a                     -- ^ Initial contents
        -> Signal (Vec n (Bool,(i,a))) -- ^ (Write enables, Write ports)
        -> Signal (Vec n i)            -- ^ Read ports
        -> Signal (Vec n a)            -- ^ Read port contents
regFile initContents writePorts readPorts =
    (\v r -> map (v !!) r) <$> registers <*> readPorts
  where
    registers     = updateVec <$> lastRegisters <*> writePorts
    lastRegisters = register initContents registers

updateVec :: (Enum i, KnownNat n) => Vec n a -> Vec n (Bool,(i,a)) -> Vec n a
updateVec vec updaters =
  foldr (\(updateEnable,updater) prevVec ->
          if updateEnable then uncurry replace updater prevVec
                          else prevVec
        ) vec updaters


instrFetch :: (Cell c, Register r, Instruction o, Num w, Integral w, Eq w,
              KnownNat dSz, KnownNat sSz, KnownNat oSz, KnownNat n, Eq r,
              Default (c r w))
           => w -> (a -> Trans (1 + dSz) sSz oSz o (c r w))
           -> Vec n a
           -> Signal (Trans (1 + dSz) sSz oSz o (c r w))
           -> (Signal (Trans (1 + dSz) sSz oSz o (c r w))
              ,Signal (Trans (1 + dSz) sSz oSz o (c r w)))
instrFetch n convert initContents pc = (head <$> x, head <$> y)
  where
    (x,y)  = instrsFetch n convert initContents (singleton <$> pc)


instrsFetch :: (Cell c, Register r, Instruction o, Num w, Integral w, Eq w,
                KnownNat dSz, KnownNat sSz, KnownNat oSz, KnownNat n, Eq r,
                Default (c r w))
            => w -> (a -> Trans (1 + dSz) sSz oSz o (c r w))
            -> Vec n a
            -> Signal (Vec m (Trans (1 + dSz) sSz oSz o (c r w)))
            -> (Signal (Vec m (Trans (1 + dSz) sSz oSz o (c r w)))
               ,Signal (Vec m (Trans (1 + dSz) sSz oSz o (c r w))))
instrsFetch n convert initContents pcs
  = (insertPCs curPC $ instructions `bypassList` nextPCTrans, nextPCTrans)
    where
      bypassList = liftA2 (zipWith bypass)
      instructions = (map convert) <$> instrMemory n initContents curPC
      curPC = (map getpc) <$> pcs
      nextPCTrans = (map (\x -> pcTrans $ x+n)) <$> curPC
      getpc t =
        do reg <- getDstPC t
           let p = getReg reg
           let x = getVal reg
           guard $ ispc p
           return x
         `catchEx` (error "instrsFetch: cant find value" )
      insertPCs pcs l = addPCs <$> pcs <*> l
      addPCs x y = zipWith addPC x y
      addPC pc (Trans d o s l) = Trans d o s (loc pc +>> l)

instrMemory :: (Integral i, KnownNat n)
            => i
            -> Vec n a
            -> Signal (Vec m i)
            -> Signal (Vec m a)
instrMemory sz arrDesc pcAddresses = (map (arrDesc !!)) <$> addrs
  where
    addrs = (map (`div` sz)) <$> pcAddresses
