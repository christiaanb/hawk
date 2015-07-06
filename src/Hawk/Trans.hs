{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hawk.Trans where

import CLaSH.Prelude

import Hawk.Cell
import Hawk.Instruction
import Hawk.Register
import Hawk.Utilities

data Trans dSz sSz iSz i c = Trans (Vec dSz c) i (Vec sSz c) (Vec iSz c)
  deriving (Eq,Show)

fill :: (Default a,KnownNat m)
     => Vec n a
     -> Vec (n + m) a
fill xs = xs ++ def

trans d o a i = Trans (fill d) o (fill a) (fill i)

pcTrans :: (Cell c,Instruction i,Register r, Default (c r w), KnownNat dSz
           ,KnownNat sSz, KnownNat iSz)
        => w -> Trans (1 + dSz) sSz iSz i (c r w)
pcTrans addr = trans (singleton $ putVal pcNothing (Just addr)) noOp Nil Nil

bypass :: (Cell c,Register r, Eq r, Eq w, Num w)
       => Trans dSz sSz iSz i (c r w)
       -> Trans dSz sSz iSz i (c r w)
       -> Trans dSz sSz iSz i (c r w)
bypass tran bypassT = if evalPredicate bypassT /= 0
                          then updSrc tran (getDst bypassT)
                          else tran

bypassDst :: (Cell c, Register r, Eq r, Eq w, Num w)
          => Trans dSz sSz iSz i (c r w) -> Trans dSz sSz iSz i (c r w)
          -> Trans dSz sSz iSz i (c r w)
bypassDst trans bypassT = if evalPredicate bypassT /= 0
                             then updDst trans (getDst bypassT)
                             else trans

getPredicate :: (Cell c,Register r)
             => Trans i dSz sSz iSz (c r w) -> Maybe (c r w)
getPredicate (Trans _ _ l _) = foldl (\b a -> if isPred a then Just a else b)
                                     Nothing l

evalPredicate :: (Cell c,Register r,Num w) => Trans dSz sSz iSz i (c r w) -> w
evalPredicate t =
   case getPredicate t of
              Just c -> if isAss c
                           then getVal c
                           else error "evalPredicate"
              Nothing -> 1

updCells :: (Cell c,Register r, Eq r, Eq w)
         => Vec n (c r w) -> Vec m (c r w) -> Vec n (c r w)
updCells cells bypassCells = repCells cellHazard cells bypassCells

repCells :: Register r
         => (c r w -> c r w -> Bool)
         -> Vec n (c r w) -> Vec m (c r w) -> Vec n (c r w)
repCells replFunc cells replacements
  = map (\cell -> foldr bypassCell cell replacements) cells
    where
      bypassCell bypassed argCell
        = if replFunc bypassed argCell
             then bypassed
             else argCell

-- | apply a function to the destination fields
repDst :: Register r
       => (c r w -> c r w -> Bool)
       -> Trans dSz sSz iSz i (c r w) -> Vec dSz (c r w)
       -> Trans dSz sSz iSz i (c r w)
repDst repFunc (Trans d o s i) cells = Trans (repCells repFunc d cells) o s i

-- | update destination fields
updDst :: (Cell c,Register r, Eq r, Eq w)
       => Trans dSz sSz iSz i (c r w) -> Vec dSz (c r w)
       -> Trans dSz sSz iSz i (c r w)
updDst = repDst cellHazard

-- | get the destination
getDst :: Register r => Trans dSz sSz iSz i (c r w) -> Vec dSz (c r w)
getDst (Trans d o s i) = d

updSrc :: (Cell c,Register r, Eq r, Eq w)
       => Trans dSz sSz iSz i (c r w)
       -> Vec n (c r w)
       -> Trans dSz sSz iSz i (c r w)
updSrc (Trans d o s i) cells = Trans d o (updCells s cells) i

-- | return the PC from the destination area
getDstPC :: (Cell c,Register r)
         => Trans dSz sSz iSz i (c r w)
         -> Maybe (c r w)
getDstPC = find isPC . getDst
