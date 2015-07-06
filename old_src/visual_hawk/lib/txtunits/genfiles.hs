fname = takeWhile (' '/=)
-- Without name: ftype = drop 3 . dropWhile (':'/=)
ftype = flip (++) "\n"

wf x = writeFile (fname x ++ ".txt") (ftype x)

main =
  do{ s <- readFile "index.txt"
    ; mapM_ wf (lines s)
    }
