\begin{code}
module STEx where
import Monad
import LazyST
\end{code}

\begin{code}
infixl 1 `handle`
\end{code}

-- Begin Signature ----------------------------------------------------------

{-
  STEx synthesizes the state and exception monads.  
-}
 

{-data STEx s a-}

{-instance Monad (STEx s)-}
{-instance MonadZero (STEx s)-}
{-instance MonadPlus (STEx s)-}

-- c `handle` x, return x if c raises an exception
\begin{code}
handle :: STEx a b -> b -> ST a b
\end{code}

-- lift an exception or st monad thing or into STEx
\begin{code}
liftEx :: Maybe a -> STEx s a
\end{code}

\begin{code}
liftST :: ST s a -> STEx s a
\end{code}

-- raise an exception if Bool is False
\begin{code}
assert :: Bool -> STEx s ()
\end{code}

-- the following functions have the same meaning as their corresponding
-- state monad functions
{-readVarSTEx :: MutVar a b -> STEx a b-}
{-writeVarSTEx :: MutVar a b -> b -> STEx a ()-}
{-newVarSTEx :: a -> STEx b (MutVar b a)-}

{-readArraySTEx :: Ix b => MutArr a b c -> b -> STEx a c-}
{-writeArraySTEx :: Ix b => MutArr a b c -> b -> c -> STEx a ()-}
{-newArraySTEx :: Ix a => (a,a) -> b -> STEx c (MutArr c a b)-}

-- End Signature -----------------------------------------------------------

\begin{code}
newtype STEx s a = STEx (ST s (Maybe a))
\end{code}

\begin{code}
instance Monad (STEx s) where
	return = STEx . return . return
	(STEx x) >>= f 
             = STEx $ do y <- x
                         case y of 
	                    Just z -> let STEx z' = f z 
                                      in z'
                            Nothing -> return Nothing
\end{code}

\begin{code}
instance MonadPlus (STEx s) where
	mzero = liftEx mzero
	mplus (STEx x) (STEx y) = STEx $ do x' <- x
					    y' <- y
					    return (mplus x' y')
\end{code}
\begin{code}
liftST x = STEx $ do {z <- x ; return $ return z}
\end{code}

\begin{code}
liftEx x = STEx $ return x
\end{code}


\begin{code}
handle (STEx m) x 
    = do y <- m 
         case y of 
	    Just z -> return z
            Nothing -> return x
\end{code}

\begin{code}
readVarSTEx v    = liftST $ readSTRef v
\end{code}

\begin{code}
writeVarSTEx v x = liftST $ writeSTRef v x
\end{code}

\begin{code}
newVarSTEx x     = liftST $ newSTRef x
\end{code}

\begin{code}
readArraySTEx v n    = liftST $ readSTArray v n
\end{code}

\begin{code}
writeArraySTEx v x n = liftST $ writeSTArray v x n
\end{code}

\begin{code}
newArraySTEx x n     = liftST $ newSTArray x n
\end{code}

{- example 
f x = do y <- liftEx x
         v <- newVarSTEx y
         readVarSTEx v

g x = runST (handle (f x) 2)
-}

\begin{code}
assert True = liftEx $ Just ()
assert False = liftEx $ Nothing
\end{code}
