\begin{code}
module Utilities where

import Maybe
import System
\end{code}


-- Begin Signature -------------------------------------------------

\begin{code}
infixr 1 `catchEx`
\end{code}

\begin{code}
catchEx :: Maybe a -> a -> a
\end{code}
\begin{code}
(><) :: (a -> b,c -> d) -> (a,c) -> (b,d)
\end{code}

-- tries to determine if the OS is windows.  Is there a better way
-- to do this?
\begin{code}
windows :: IO Bool
\end{code}

-- End Signature -------------------------------------------------

\begin{code}
catchEx = flip fromMaybe
\end{code}
\begin{code}
(f,g) >< (x,y) = (f x, g y)
\end{code}


\begin{code}
windows
   = do { s <- getEnv "OS"
        ; return $ case s of
		'W':'i':'n':_ -> True
		_ -> False
        }
\end{code}
