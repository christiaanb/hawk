<H3>Main.lhs</H3>

\begin{code}

module Main(main) where

import System(getArgs)
import Hawk(hawkMain)
import Pipeline(pipeline)
import Signal(view)
import PipeUtils(printAssm)
\end{code}

\begin{code}
main 
  = do args <- getArgs
       case args of
          [f]          -> hawkMain f (mapM print . view . pipeline)
--          ["-count",f] -> hawkMain f (print . length . view . pipeline)
--          ["-page",f]  -> hawkMain f (page . pipeline)
--          otherwise    -> error "usage: prog [-count | -page] filename\n"
          otherwise    -> error "usage: simulate filename\n"
       return ()
\end{code}

--go :: String -> IO ()
--go filename = hawkMain filename (page . pipeline)

