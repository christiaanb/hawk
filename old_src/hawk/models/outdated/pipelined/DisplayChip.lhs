\begin{code}

module DisplayChip(page) where

import Hawk
import DLX
import Trans

getPC (Trans [Reg PC (Val pc)] _ _ _) = show pc
getPC x = "no value"

printChip :: [DLXTrans] -> IO()
printChip (pc:instr1:instr2:instr3:instr4:other) =
	do putStrLn  $ "================================================="
	   putStrLn  $ "PC = " ++ (getPC pc)
	   putStrLn  $ ""
	   putStrLn  $ "IF    ------------"
	   putStrLn  $ show instr1
	   putStrLn  $ ""
	   putStrLn  $ "RF    ------------"
	   putStrLn  $ show instr2
	   putStrLn  $ ""
	   putStrLn  $ "EX    ------------"
	   putStrLn  $ show instr3
	   putStrLn  $ ""
	   putStrLn  $ "WB    ------------"
	   putStrLn  $ show instr4
	   putStrLn  $ ""
	   printOther other
printChip (pc:wb:other) =
        do putStrLn  $ "================================================="
           putStrLn  $ "PC = " ++ (getPC pc)
           putStrLn  $ ""
           putStrLn  $ show wb
           printOther other

printOther [] = return ()
printOther l = do putStrLn $ "Other ------------"
		  mapM_ (putStrLn . show) l
                  putStrLn $ ""
                  putStrLn $ ""
	  

page (List (x:t)) = displayChip 1 (repeat x) x t

displayChip  n (b:bs) x (f:fs)
  = do printChip x
       putStrLn $ "[return,b,q] (clock = " ++ (show n) ++ ")"
       c <- getChar
       case c of 
	  'b' -> displayChip (n-1) bs b (x:f:fs)
          'q' -> return ()
          _ -> displayChip (n+1) (x:b:bs) f fs

\end{code}
