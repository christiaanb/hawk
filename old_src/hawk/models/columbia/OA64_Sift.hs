module OA64_Sift(sift_trans) where

import Hawk
import OA64

sift_trans k xs = unbundleList $ lift1 (sift kl) xs
  where
  kl = take k $ repeat []

sift ls [] = ls
sift ls (t:ts) = let n = getCompute t `mod` length ls
                   in sift (place n t ls) ts


place n x (l:ls)
     | n<1       = (l ++ [x]):ls
     | otherwise = l:place (n-1) x ls
place _ _ []     = []

