<H3>Words.lhs</H3>

\begin{code}
module Words where

import Word
import Ix
\end{code}

-- Begin Signature ---------------------------------------------------------
<P>
The Word class captures both the common operations and class instances
that you would want from words of different sizes.  


\begin{code}
class (Ix w,Num w,Integral w,Bounded w, Eq w) => Word w where
  --intToWord       :: Int -> w
  num_half        :: w
  num_bytes       :: w
  max_signed      :: w
  min_signed      :: w
  max_signed_half :: w
  sign            :: w -> w
  unsign          :: w -> w
  --toWord          :: Integral a => a -> w
  sign_half       :: w -> w

  --toWord      = intToWord . toInt
  sign_half n = n `signedModulus` num_half
\end{code}

\begin{code}
class Word w => Word2 i w where
  toWord :: i -> w
\end{code}

\begin{code}
{-instance Word Int-}
{-instance Word Word8-}
{-instance Word Word32-}
\end{code}

This isn't set yet because Word64 is not set to Num in Hugs.
\begin{code}
-- instance Word Word64 
\end{code}

-- End Signature ---------------------------------------------------------




\begin{code}
instance Word Int where
  num_half        = mk_num_half 31
  num_bytes       = mk_num_bytes 31
  max_signed      = mk_max_signed 31
  min_signed      = mk_min_signed 31
  max_signed_half = mk_max_signed_half 31
  sign            = mk_sign 31
  unsign          = mk_unsign 31
\end{code}

\begin{code}
instance Integral i => Word2 i Int where
  toWord          = toInt
\end{code}

\begin{code}
instance Word Word8 where
  num_half        = mk_num_half 8 
  num_bytes       = mk_num_bytes 8      
  max_signed      = mk_max_signed 8
  min_signed      = mk_min_signed 8
  max_signed_half = mk_max_signed_half 8
  sign            = mk_sign 8
  unsign          = mk_unsign 8
\end{code}

\begin{code}
instance Integral i => Word2 i Word8 where
  toWord          = intToWord8 . toInt
\end{code}


\begin{code}
instance Word Word32 where
  num_half        = mk_num_half 32
  num_bytes       = mk_num_bytes 32      
  max_signed      = mk_max_signed 32
  min_signed      = mk_min_signed 32
  max_signed_half = mk_max_signed_half 32
  sign            = mk_sign 32
  unsign          = mk_unsign 32
\end{code}


\begin{code}
instance Integral i => Word2 i Word32 where
  toWord          = intToWord32 . toInt
\end{code}


\begin{code}
mk_num_half  x       = 2^(x `div` 2)
\end{code}

\begin{code}
mk_num_bytes x       = 2^(x `div` 4)
\end{code}

\begin{code}
mk_max_signed x      = 2^(x-1) - 1
\end{code}

\begin{code}
mk_min_signed x      = -2^(x-1)
\end{code}

\begin{code}
mk_max_signed_half x = 2^((x `div` 2) - 1) - 1
\end{code}

\begin{code}
mk_sign x n          = fromInteger $ n' `signedModulus` ((2^x)::Integer)
   where n' = toInteger n
\end{code}

\begin{code}
mk_unsign x n        = fromInteger $ if n' >=0 then n' else n' + 2^x'
   where n' = toInteger n
         x' = toInteger x 
\end{code}

\begin{code}
signedModulus x m
  = if modNum <= (m `div` 2) - 1
      then modNum
      else m - modNum
    where
      modNum = x `mod` m
\end{code}

