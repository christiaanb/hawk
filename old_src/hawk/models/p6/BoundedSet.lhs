<H3>BoundedSet.lhs for P6 Case Study</H3>

\begin{code}
module BoundedSet
  ( new
  , readBound
  , readSize
  , read  
  , clear
  , insert
  , spaceAvail
  , rmSuch 
  , rmSuchN
  , BoundedSet
  , iterateSet
  ) where

import LazyST
import Prelude hiding (read)
import List(partition)
\end{code}

\begin{code}
new        :: Int -> ST s (BoundedSet s a)
\end{code}

\begin{code}
readBound  :: BoundedSet s a -> ST s Int
\end{code}

\begin{code}
readSize   :: BoundedSet s a -> ST s Int
\end{code}

\begin{code}
read       :: BoundedSet s a -> ST s [a]
\end{code}

\begin{code}
clear      :: BoundedSet s a -> ST s [a]
\end{code}

\begin{code}
insert     :: BoundedSet s a -> [a] -> ST s ()
\end{code}

\begin{code}
spaceAvail :: BoundedSet s a -> ST s Int
\end{code}

\begin{code}
rmSuch     :: BoundedSet s a -> (a -> Bool) -> ST s [a]
\end{code}

\begin{code}
rmSuchN    :: BoundedSet s a -> Int -> (a -> Bool) -> ST s [a]
\end{code}

\begin{code}
iterateSet :: BoundedSet s a -> (a -> a) -> ST s ()
\end{code}

<!-- ---------------------------------------------------- -->

Implementation

\begin{code}
type BoundedSet s a = (STRef s [a],Int)
\end{code}

\begin{code}
iterateSet s f =
   do { set <- read s
      ; write s (map f set)
      }
\end{code}

\begin{code}
read (s,n) = readSTRef s
\end{code}

\begin{code}
rmSuch s f
  = do { set <- read s
       ; let (yes,no) = partition f set
       ; write s no
       ; return yes
       }
\end{code}

\begin{code} 
rmSuchN s n f 
  = do { such <- rmSuch s f
       ; let (big,small) = splitAt n such
       ; insert s small
       ; return big
       }
\end{code}

\begin{code}
write    :: BoundedSet s a -> [a] -> ST s ()
write (s,n) x = writeSTRef s x
\end{code}


\begin{code}
readBound (s,n) = return n
\end{code}

\begin{code}
new n 
  = do { set <- newSTRef []
       ; return (set,n)
       }
\end{code}

\begin{code}
clear s =
  do { set <- read s
     ; write s []
     ; return set
     }
\end{code}

\begin{code} 
readSize s =
  do { set <- read s
     ; return ( length set)
     }
\end{code}
	
\begin{code} 
spaceAvail s
  = do { bnd <- readBound s
       ; sz  <- readSize s
       ; return (bnd - sz)
       }
\end{code}
      

\begin{code} 
insert s l
  = do { set <- read s
       ; n <- readBound s
       ; write s $ take n (set ++ l)
       }
\end{code}



