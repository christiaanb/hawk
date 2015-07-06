<H3> Symbo.lhs for p6 Case Study</H3>

\begin{code}
module Symbo where
\end{code}

\begin{code}
import Ix
import Words
\end{code}

\begin{code}
data Symbo a =
      Const a
    | Var String
    | Plus (Symbo a) (Symbo a)
    | Minus (Symbo a) (Symbo a)
    | Times (Symbo a) (Symbo a)
    | Negate (Symbo a)
    | Sign (Symbo a)
    | Unsign (Symbo a)
    deriving Eq
\end{code}

\begin{code}
unConst (Const x) = x
unConst _ = error "Symbo: unConst"
snaive_lift1 f (Const x) = Const (f x)
snaive_lift1 f _ = error "Symbo: snaive_lift1"
snaive_lift2 f (Const x) (Const y) = Const (f x y)
snaive_lift2 f _ _ = error "Symbo: snaive_lift2"
slift2 c s (Const x) (Const y) = Const (c x y)
slift2 c s x y = s x y
\end{code}

\begin{code}
instance Show a => Show (Symbo a) where
    showsPrec p (Const a) = showsPrec p a
    showsPrec p (Var s) = showString s
    showsPrec p (Plus x y) =
	showParen (p > 6) (showsPrec 6 x .
			   showString " + " .
			   showsPrec 6 y)
    showsPrec p (Minus x y) =
	showParen (p > 6) (showsPrec 6 x .
			   showString " - " .
			   showsPrec 6 y)
    showsPrec p (Times x y) =
	showParen (p > 7) (showsPrec 7 x .
			   showString " * " .
			   showsPrec 7 y)
    showsPrec p (Negate x) =
	showParen (p > 9) (showString "-" . showsPrec 9 x)
    showsPrec p (Sign x) = showsPrec p x
    showsPrec p (Unsign x) = showsPrec p x
\end{code}

\begin{code}
instance Functor Symbo where
    fmap f (Const x) = Const (f x)
    fmap f (Var x) = Var x
    fmap f (Plus x y) = Plus (fmap f x) (fmap f y)
    fmap f (Minus x y) = Minus (fmap f x) (fmap f y)
    fmap f (Times x y) = Times (fmap f x) (fmap f y)
    fmap f (Negate x) = Negate (fmap f x)
    fmap f (Sign x) = Sign (fmap f x)
    fmap f (Unsign x) = Unsign (fmap f x)
\end{code}

\begin{code}
instance Bounded a => Bounded (Symbo a) where
    minBound = Const minBound
    maxBound = Const maxBound
\end{code}

\begin{code}
instance Integral a => Integral (Symbo a) where
    quot = snaive_lift2 quot
    rem = snaive_lift2 rem
    div = snaive_lift2 div	
    mod = snaive_lift2 mod
    quotRem (Const x) (Const y) = let (z, w) = quotRem x y in (Const z, Const w)
    divMod (Const x) (Const y) = let (z, w) = divMod x y in (Const z, Const w)
    toInteger (Const x) = toInteger x
    toInteger _ = error "Symbo: toInteger"
    toInt (Const x) = toInt x
    toInt _ = error "Symbo: toInt"
\end{code}

\begin{code}
instance Enum a => Enum (Symbo a) where
    toEnum x = Const (toEnum x)
    fromEnum (Const x) = fromEnum x
    enumFrom (Const x) = map Const (enumFrom x)
    enumFromTo (Const x) (Const y) = map Const (enumFromTo x y)
    enumFromThen (Const x) (Const y) = map Const (enumFromThen x y)
    enumFromThenTo (Const x) (Const y) (Const z) = map Const (enumFromThenTo x y z)
\end{code}

\begin{code}
instance Real a => Real (Symbo a) where
    toRational x = error "Symbo: toRational"
\end{code}

\begin{code}
instance Ord a => Ord (Symbo a) where
    compare (Const x) (Const y) = compare x y
\end{code}

\begin{code}
instance Num a => Num (Symbo a) where
    Const x + Const y = Const (x + y)
    Const 0 + y = y
    x + Const 0 = x
    x + y = x `Plus` y

    Const x - Const y = Const (x - y)
    x - Const 0 = x
    x - y = x `Minus` y

    Const x * Const y = Const (x * y)
    x@(Const 0) * y = x
    x * y@(Const 0) = y
    Const 1 * y = y
    x * Const 1 = x
    x * y = x `Times` y

    negate (Const x) = Const (negate x)
    negate (Negate x) = x
    negate x = Negate x

    abs = snaive_lift1 abs
    signum = snaive_lift1 signum
    fromInteger = Const . fromInteger
    fromInt = Const . fromInt
\end{code}

\begin{code}
instance Ix a => Ix (Symbo a) where
    range (Const x, Const y) = map Const (range (x, y))
    index (Const x, Const y) (Const z) = index (x, y) z
    inRange (Const x, Const y) (Const z) = inRange (x, y) z
\end{code}

\begin{code}
instance Word w => Word (Symbo w) where
    num_half          = Const num_half
    num_bytes         = Const num_bytes
    max_signed        = Const max_signed
    min_signed        = Const min_signed
    max_signed_half   = Const max_signed_half
    sign (Const x)    = Const (sign x)
    sign x            = Sign x
    unsign (Const x)  = Const (unsign x)
    unsign x          = Unsign x
    --toWord            = Const . toWord
\end{code}

\begin{code}
instance Word2 i w => Word2 (Symbo i) (Symbo w) where
    toWord = fmap toWord
\end{code}
