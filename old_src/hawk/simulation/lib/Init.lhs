\begin{code}
module Init where

import Signal
\end{code}

-- Begin Signature ------------------------------------------------------
{-

Very often, particularily when operating over pointed domains, each
type has a particular value that serves well as an inital state. 
The Init class picks that value out.  For example, the "def" value
for lists is "[]"

-}

\begin{code}
class Init a where
   def :: a

{-instance Init [a]-}
{-instance Init (Maybe a)-}
{-instance Init Int-}
{-instance Init Bool-}
\end{code}


-- delay a signal using the type's default value as the initializer
\begin{code}
del :: Init a => Signal a -> Signal a
\end{code}


-- End Signature ------------------------------------------------------
   
\begin{code}
instance Init [a] where
   def = []

instance Init (Maybe a) where
   def = Nothing

instance Init Int where
   def = 0

instance Init Bool where
   def = False
\end{code}

\begin{code}
del x = delay def x
\end{code}
