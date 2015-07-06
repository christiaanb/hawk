module Main where

import System(getArgs)
import Hawk
import Columbia(columbia)
import Signal
import System

main =
 do clearProbes_UNIX
    args <- getArgs
    case args of
        [file]    -> run file 
        ["-count",file]    
             -> hawkMain file (print . length . view . columbia)
        ["-sample",n,file] -> sample (read n) file
        otherwise    -> error "usage: prog [-count | -page] filename\n"
    return ()

run file =  hawkMain file (mapM_ (putStrLn . show) . (view . columbia))
sample n file =
  do clearProbes_UNIX
     hawkMain file (mapM_ (putStrLn . show) . (take n . view . columbia))

