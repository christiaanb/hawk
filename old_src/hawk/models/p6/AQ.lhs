<H3>AQ.lhs for P6 Case Study</H3>

Addressable Queues

\begin{code}
module AQ where
import LazyST
import Utils

import Hawk
\end{code}

\begin{code}
type AQ s a  = (STArray s Int (Maybe a), Front s,Back s,QSize s,Int)
type Front s = STRef s Int
type Back s  = STRef s Int
type QSize s = STRef s Int
type QAddr   = Int
\end{code}

\begin{code}
new          :: Int ->  ST s (AQ s a)
\end{code}

\begin{code}
enQueue      :: AQ s a -> a -> ST s QAddr
\end{code}

\begin{code}
deQueue      :: AQ s a ->  ST s (a,QAddr)
\end{code}

\begin{code}
reQueue      :: AQ s a -> a ->  ST s QAddr
\end{code}

\begin{code}
getSize      :: AQ s a ->  ST s Int
\end{code}

\begin{code}
getMax       :: AQ s a ->  ST s Int
\end{code}

\begin{code}
deQueueWhile :: AQ s a -> (a -> Bool) ->  ST s [a]
\end{code}

\begin{code}
enList       :: AQ s a -> [a] -> ST s [QAddr]
\end{code}

\begin{code}
update       :: AQ s a -> QAddr -> (a -> a) -> ST s ()
\end{code}

\begin{code}
clear        :: AQ s a ->  ST s ()
\end{code}

\begin{code}
space        :: AQ s a -> ST s Int
\end{code}

<HR>
<!-- ------------------------------------------------------------ -->

\begin{code}
assertM True _ = return ()
assertM False s = error $ s  ++ "\n"
\end{code}

\begin{code} 
insert x y z = setQVal x y (Just z)
\end{code}

\begin{code} 
new n
  = do { q <- newSTArray (0,n) Nothing
       ; f <- newSTRef (-1)
       ; b <- newSTRef 0
       ; s <- newSTRef 0
       ; return (q,f,b,s,n)
       }
\end{code}

\begin{code} 
clear (q,f,b,s,n)
  = do { mapM (\x -> writeSTArray q x Nothing) [0 .. n]
       ; writeSTRef f (-1)
       ; writeSTRef b 0
       ; writeSTRef s 0
       }
\end{code}
   
\begin{code} 
enQueue q elem
  = do { sz <- getSize q
       ; max <- getMax q
       ; () <- assertM (sz < max) "enQueue over max"
       ; f <- getFront q
       ; let f' = (f+1) `mod` max
       ; setQVal q f' (Just elem)
       ; setSize q (sz+1)
       ; setFront q f'
       ; return f'
       }
\end{code}

\begin{code} 
reQueue q elem
  = do { sz <- getSize q
       ; max <- getMax q
       ; assertM (sz < max) "reQueue over max"
       ; b <- getBack q
       ; let b' = (b-1) `mod` max
       ; setQVal q b' (Just elem)
       ; setSize q (sz+1)
       ; setBack q b' 
       ; return b'
       }
\end{code}

\begin{code} 
deQueue q 
  = do { sz <- getSize q
       ; max <- getMax q
       ; assertM (sz > 0) "deQueue under min"
       ; b <- getBack q
       ; mj <- getQVal q b
       ; let j = mj `catchEx` error "deQueue"
       ; setSize q (sz-1)
       ; setBack q $ (b+1) `mod` max
       ; return (j,b)
       }
\end{code}

\begin{code} 
space q
  = do { sz <- getSize q
       ; m  <- getMax q
       ; return $ m - sz
       } 
\end{code}

\begin{code} 
deQueueWhile q f
  = do { sz <- getSize q
       ; if (sz < 1) 
           then return []
           else do { (elem,addr) <- deQueue q
                   ; if (f elem) 
                       then do { elems <- deQueueWhile q f
                               ; return (elem:elems)
                               }
                       else do { reQueue q elem
                               ; return []
                               }
                   }
       }
\end{code}



\begin{code} 
enList q [] = return []
enList q (x:xs)
  = do { sz <- space q
       ; if (sz > 0) 
           then do { a <- enQueue q x
                   ; l <- enList q xs
                   ; return $ a:l
                   }
           else return []
       }
\end{code}

\begin{code} 
assignAddrs q l
  = do { let len = length l
       ; sz <- space q
       ; max <- getMax q
       ; assertM (sz >= len) "sz < len"
       ; f <- getFront q
       ; let f' = f+1
       ; let addrs = map (`mod` max) [f' .. f'+len]
       ; return $ zip l addrs
       }
\end{code}

\begin{code} 
assignAddr q x 
  = do { ans <- assignAddrs q [x]
       ; return $ head ans
       }
\end{code}

\begin{code} 
iterateQueue q f  
  = do { front <- getFront q
       ; back <- getBack q
       ; max <- getMax q
       ; updateWhile q front front back max f
       }
  where updateWhile q front n back max f
          | n == back = return ()
          | otherwise = do { val <- getQVal q n
                           ; val <- case val of
                                       Just x -> return $ Just $ f x
                                       Nothing -> return Nothing
                           ; setQVal q n val
                           ; updateWhile q front ((n+1) `mod` max) back max f
                           }
\end{code}

\begin{code} 
update q n f 
  = do { x <- getQVal q n
       ; setQVal q n $ fmap f x
       }
\end{code}

<HR>
<!-- -------------------------------------------------------------------- -->

\begin{code} 
getSize  (q,f,b,s,m)     = readSTRef s
\end{code}

\begin{code} 
setSize  (q,f,b,s,m) v   = writeSTRef s v
\end{code}

\begin{code}
getMax   (q,f,b,s,m)     = return m
\end{code}

\begin{code}
getFront (q,f,b,s,m)     = readSTRef f
\end{code}

\begin{code}
setFront (q,f,b,s,m) v   = writeSTRef f v
\end{code}

\begin{code}
getBack  (q,f,b,s,m)     = readSTRef b
\end{code}

\begin{code}
setBack  (q,f,b,s,m) v   = writeSTRef b v
\end{code}

\begin{code}
getQVal  (q,f,b,s,m) n   = readSTArray q n
\end{code}

\begin{code}
setQVal  (q,f,b,s,m) n e = writeSTArray q n e
\end{code}
        
