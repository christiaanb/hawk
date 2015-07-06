{-
match :: [Graph] -> (([Graph], IVShape ()), ([Graph], IVShape ())) -> IO (([Graph], IVShape ()), ([Graph], IVShape ()))
match orig ((a, a'), (b, b'))
  | graphEq orig a = return ((a, a'), (b, b'))
  | graphEq orig b = return ((b, b'), (a, a'))
  | otherwise      = ioError (userError "Match failed")

setWire :: IVShape () -> Graph -> IO ()
setWire group (Wire name begin end) =
  do{ shapes <- getShapesVS group
    ; ss     <- (items getCountVS getItemVS) shapes
    ; mapM_ rePosition ss
    }
  where rePosition shape =
          do{ name' <- getData1VS shape
            ; guardIO (name == name')
            ; setPosition begin shape "Begin"
            ; setPosition end   shape "End"
            } `catch` (\a -> return ())
        setPosition "" shape w = return ()
        setPosition f  shape w =
          do{ setFormulaVC f =<< getCellsVS (w ++ "X") shape
            ; setFormulaVC f =<< getCellsVS (w ++ "Y") shape
            }
-}
-----------------------------------------------------------
-- Graphs

data Graph = Node {nodeID :: String}
           | Wire {wireID :: String, begin, end :: String} deriving Show

-- [FIXME: not entirely correct]
unifyWires :: [Graph] -> [Graph] -> [Graph]
unifyWires wild fixed =
  [ Wire n (if wildcard b' then b else "") (if wildcard e' then e else "")
  | m@(Wire n b e) <- map (f fixed) ws, w@(Wire n' b' e') <- ws, n == n' ]
  where ws = [ y | y@(Wire n b e) <- wild ]

f ws w = foldl bar w [ y | y@(Wire n b e) <- ws ]

bar :: Graph -> Graph -> Graph
bar (Wire n ('P':bs) ('P':es)) (Wire _ _ _) = Wire n ('P':bs) ('P':es)
bar (Wire n ('P':bs) es      ) (Wire _ ('P':bs') es')
  | eqWire bs bs' = Wire n ('P':bs) es'
  | otherwise     = Wire n ('P':bs) es
bar (Wire n bs       ('P':es)) (Wire _ bs' ('P':es'))
  | eqWire es es' = Wire n bs' ('P':es)
  | otherwise     = Wire n bs ('P':es)
bar (Wire n bs       es      ) (Wire _ bs' es') = Wire n bs'      es'


-- [FIXME: to liberal]
graphEq :: [Graph] -> [Graph] -> Bool
graphEq xs ys = length xs == length ys && sort (nodes xs) == sort (nodes ys)
  where nodes as = [ n | a@(Node n) <- as ]
        edges as = [ a | a@(Wire _ _ _) <- as ]
        safeEdges as = [ a | a@(Wire _ b e) <- as, not (wildcard b), not (wildcard e)]

mkGraph :: IVShape () -> IO [Graph]
mkGraph shape =
  do{ shapes <- getShapesVS shape
    ; ss     <- (items getCountVS getItemVS) shapes
    ; mapM shapeToGraph ss
    }

shapeToGraph :: IVShape a -> IO Graph
shapeToGraph shape =
  do{ name  <- getData1VS shape
    ; oneD  <- getOneDVS shape
    ; if oneD == 0 then
        return (Node name)
       else
        do{ start <- getFormulaVC =<< getCellsVS "BeginX" shape
          ; stop  <- getFormulaVC =<< getCellsVS "EndX" shape
          ; return (Wire name start stop)
          }
    }

eqGraph :: Graph -> Graph -> Bool
eqGraph (Wire _ b e) (Wire _ b' e') = eqWire b b' && eqWire e e'
eqGraph (Node n)     (Node n')       = n == n'
eqGraph _            _               = False

-- P is magic because all real connections start with ""PAR(PNT(..."
-- and unconnected wires start with a number "1.3in"
eqWire :: String -> String -> Bool
eqWire (c:cs) (c':cs') = c /= 'P' || c' /= 'P' || filtered cs == filtered cs'
eqWire "" "" = True
eqWire _  _  = False

filtered :: String -> String
filtered "" = ""
filtered ('.':cs) = filtered (dropWhile isDigit cs)
filtered (c:cs)   = c:filtered cs

wildcard :: String -> Bool
wildcard ""     = False
wildcard (c:cs) = c /= 'P'
-----------------------------------------------------------
-- Graphs
--     ; grto <- mkGraph to
--     ; writeFile "C:\\hawk\\visual_hawk\\lib\\test.hs"
--                ("Old: " ++ show grold ++ "\n" ++ "New: " ++ show grnew ++ "\n" ++ "Rewrite: " ++ show grto)

-- RegFile law
g1 = [Wire{wireID="Instruction.O1",begin="PAR(PNT(regFile!Connections.O1_Trans.X,regFile!Connections.O1_Trans.Y))",end="Sheet.1!Width*1"},Wire{wireID="WriteBack.I2",begin="Sheet.1!Width*1",end="PAR(PNT(regFile!Connections.I2_Trans.X,regFile!Connections.I2_Trans.Y))"},Node{nodeID="regFile"},Wire{wireID="Instruction.I1",begin="Sheet.1!Width*0",end="PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))"}]

-- From users drawing
g2 = [Node{nodeID="regFile"},Wire{wireID="",begin="PAR(PNT(Sheet.5!Connections.O1_Trans.X,Sheet.5!Connections.O1_Trans.Y))",end="PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))"},Wire{wireID="",begin="PAR(PNT(regFile!Connections.O1_Trans.X,regFile!Connections.O1_Trans.Y))",end="PAR(PNT(Sheet.7!Connections.I1_Trans.X,Sheet.7!Connections.I1_Trans.Y))"},Wire{wireID="",begin="PAR(PNT(Sheet.3!Connections.O1_.X,Sheet.3!Connections.O1_.Y))",end="PAR(PNT(regFile!Connections.I2_Trans.X,regFile!Connections.I2_Trans.Y))"}]

g3 = [Wire{wireID="Instruction.O1",begin="",end="Sheet.1!Width*1"},Wire{wireID="WriteBack.I2",begin="Sheet.1!Width*1",end=""},Wire{wireID="Instruction.I1",begin="Sheet.1!Width*0",end=""}]

w1 = Wire{wireID="Instruction.I1",begin="Sheet.1!Width*0",end="PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))"}

w2 = Wire{wireID="",begin="PAR(PNT(Sheet.5!Connections.O1_Trans.X,Sheet.5!Connections.O1_Trans.Y))",end="PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))"}

w3 = Wire{wireID="Instruction.I1",begin="Sheet.1!Width*0",end="PAR(PNT(delay!Connections.I1_Trans.X,delay!Connections.I1_Trans.Y))"}


-----------------------------------------------------------
-- Old Experimental Code

{-
bfoo :: IO ()
bfoo  =
  do  visio' <- getActiveObject "Visio.Application"
      visio  <- getApplication visio'
      p <- getActivePage visio
      w <- getActiveWindow visio
      copy w
      sel <- getSelection w
      item <- getItem3 1 sel
      pinx <- getCellFormula "PinX" item
      piny <- getCellFormula "PinY" item
   ---   print pinx
   ---
      s <- drawRectangle 1 1 1 1 p
      setCells' "PinX" pinx s
      setCells' "PinY" piny s
      setCells' "Height" "1" s
      setCells' "Width" "1" s
      setCells' "FillForegnd" "13" s
      return ()
--  `catch` (\a -> return ())
  `catch` (\a -> do writeFile "\\err.txt" (show a )
                    system "\\winnt\\system32\\notepad.exe \\err.txt"
                    system "del \\err.txt"
                    return ()
          )

setCells' s st val =
  do c <- getCells st s
     setFormula val c

getCellFormula s i =
  do x <-  getCells s i
     getFormula x
-}


-----------------------------------------------------------
-- New Graphs

type GraphG = [Edge]

data Edge = Edge {edgeID :: String, edgeType :: String, from, to :: Term} deriving (Show)

instance Eq Edge where
  (Edge _ t b e) == (Edge _ t' b' e') = edgeTypeEq t t' && b == b' && e == e'

edgeTypeEq :: String -> String -> Bool
edgeTypeEq s s' = isPrefixOf s s' || isSuffixOf s s' || isPrefixOf s' s || isSuffixOf s' s

type Subst = [(String, String)]

data Term = Var String
          | Const String deriving (Show, Ord, Eq)

renameS :: GraphG -> GraphG -> [Subst]
renameS [] [] = [emptySubst]
renameS os (n:ns) =
  [ s++ss | o <- os
          , edgeTypeEq (edgeType o) (edgeType n)
          , s <- mkSubst o n
          , ss <- renameS (os \\ [o]) (subst s ns)
  ]

emptySubst = []

mkSubst :: Edge -> Edge -> [Subst]
mkSubst (Edge i t c1 c2) (Edge i' t' b e) = [ s++s' | s <- mkSubst' c1 b, s' <- mkSubst' c2 e ]

mkSubst' :: Term -> Term -> [Subst]
mkSubst' (Const c) (Const c') = []
mkSubst' (Const c) (Var v) = [[(v, c)]]

subst :: Subst -> GraphG -> GraphG
subst [] es = es
subst (s:ss) es = subst ss [ Edge i t (subst' s b) (subst' s e) | Edge i t b e <- es ]

subst' :: (String, String) -> Term -> Term
subst' (v, c) (Const c') = Const c'
subst' (v, c) (Var v')
  | v == v'   = Const c
  | otherwise = Var v'

trans :: GraphG -> GraphG -> [Subst]
trans os ns = [ s | s <- renameS os ns, null (subst s ns \\ os) ]

matchG :: GraphG -> GraphG -> Bool
matchG os ns = not (null (trans os ns))

boundary :: GraphG -> GraphG -> [GraphG]
boundary os ns = [ subst (map (\(a,b)->(b,a)) s) es | s <- trans os ns ]
  where es = [ Edge i t (fix b) (fix e) | Edge i t b e <- os, wildcard b || wildcard e ]
        fix (Const s) = if wildcard' s then Var s else Const ""
        wildcard (Const s) = wildcard' s
        wildcard' s = not (isPrefixOf "PAR(PNT(" s)

mkGraph :: (String -> Term) -> IVSelection () -> IO GraphG
mkGraph f select =
  do{ ss <- (items countVS itemVS) select
    ; mG <- mapM (shapeToEdge f select) ss
    ; return (catMaybes mG)
    }

shapeToEdge :: (String -> Term) -> IVSelection () -> IVShape () -> IO (Maybe Edge)
shapeToEdge f select shape =
  do{ oneD  <- getOneDVS shape
    ; guardIO (oneD /= 0)
    ; name  <- getData1VS shape
    ; (b, bt) <- getType shape "Begin"
    ; (e, et) <- getType shape "End"
    ; return (Just (Edge name (bt ++ "->" ++ et) (f b) (f e)))
    } `catch` (\a -> return Nothing)

-- returns 
getType :: IVShape () -> String -> IO (String, String)
getType shape s =
  do{ px <- getFormulaVC =<< getCellsVS (s ++ "X") shape
    ; py <- getFormulaVC =<< getCellsVS (s ++ "Y") shape
    ; let (n, t) = parseType px
    ; let t' = takeWhile (/='.') n  -- FIXME: Should get the Data1 from the 'n' shape
    ; return (px ++ "@" ++ py, t' ++ t)
    }

-- "PAR(PNT(name!Connections.I1_SignalInt.X,name!Connections.I1_SignalInt.Y))"
-- or "1.23in."

parseType :: String -> (String, String)
parseType s
  | isPrefixOf "PAR(PNT(" s =
     let name = takeWhile ('!'/=) (drop 8 s)
         port = drop 13 $ dropWhile ('!'/=) s
         i    = takeWhile ('_'/=) port
         ttyp = takeWhile ('.'/=) (tail $ dropWhile ('_'/=) port)
      in (name, i ++ "." ++ ttyp)
  | otherwise = ("", "")

-------------------------------------------------------
-- example graphs
{-
gold = 
  [Edge{edgeID=""
       ,edgeType="instrsO1.Trans->regFileI1.Trans"
       ,from=Var "PAR(PNT(instrs!Connections.O1_Trans.X,instrs!Connections.O1_Trans.Y))@PAR(PNT(instrs!Connections.O1_Trans.X,instrs!Connections.O1_Trans.Y))"
       ,to=Var "PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))@PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))"}
  ,Edge{edgeID=""
       ,edgeType="regFileO1.Trans->ALUI1.Trans"
       ,from=Var "PAR(PNT(regFile!Connections.O1_Trans.X,regFile!Connections.O1_Trans.Y))@PAR(PNT(regFile!Connections.O1_Trans.X,regFile!Connections.O1_Trans.Y))"
       ,to=Var "PAR(PNT(ALU!Connections.I1_Trans.X,ALU!Connections.I1_Trans.Y))@PAR(PNT(ALU!Connections.I1_Trans.X,ALU!Connections.I1_Trans.Y))"}
  ,Edge{edgeID=""
       ,edgeType="idO1.->regFileI2.Trans"
       ,from=Var "PAR(PNT(id!Connections.O1_.X,id!Connections.O1_.Y))@PAR(PNT(id!Connections.O1_.X,id!Connections.O1_.Y))"
       ,to=Var "PAR(PNT(regFile!Connections.I2_Trans.X,regFile!Connections.I2_Trans.Y))@PAR(PNT(regFile!Connections.I2_Trans.X,regFile!Connections.I2_Trans.Y))"}]

glhs = 
  [Edge{edgeID="Instruction.O1"
       ,edgeType="regFileO1.Trans->"
       ,from=Const "PAR(PNT(regFile!Connections.O1_Trans.X,regFile!Connections.O1_Trans.Y))@PAR(PNT(regFile!Connections.O1_Trans.X,regFile!Connections.O1_Trans.Y))"
       ,to=Const "4.875 in.@11.125 in."}
  ,Edge{edgeID="WriteBack.I2"
       ,edgeType="->regFileI2.Trans"
       ,from=Const "4.875 in.@9.75 in."
       ,to=Const "PAR(PNT(regFile!Connections.I2_Trans.X,regFile!Connections.I2_Trans.Y))@PAR(PNT(regFile!Connections.I2_Trans.X,regFile!Connections.I2_Trans.Y))"}
  ,Edge{edgeID="Instruction.I1"
       ,edgeType="->regFileI1.Trans"
       ,from=Const "0.75 in.@11.125 in."
       ,to=Const "PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))@PAR(PNT(regFile!Connections.I1_Trans.X,regFile!Connections.I1_Trans.Y))"}]

grhs =
  [Edge{edgeID="Instruction.I1"
       ,edgeType="->regFileI1.Trans"
       ,from=Const "0.75 in.@8.625 in."
       ,to=Const "PAR(PNT(regFile.16!Connections.I1_Trans.X,regFile.16!Connections.I1_Trans.Y))@PAR(PNT(regFile.16!Connections.I1_Trans.X,regFile.16!Connections.I1_Trans.Y))"}
  ,Edge{edgeID="Instruction.O1"
       ,edgeType="bypassO1.Trans->"
       ,from=Const "PAR(PNT(bypass!Connections.O1_Trans.X,bypass!Connections.O1_Trans.Y))@PAR(PNT(bypass!Connections.O1_Trans.X,bypass!Connections.O1_Trans.Y))"
       ,to=Const "4.875 in.@8.625 in."}
  ,Edge{edgeID=""
       ,edgeType="regFileO1.Trans->bypassI1.Trans"
       ,from=Const "PAR(PNT(regFile.16!Connections.O1_Trans.X,regFile.16!Connections.O1_Trans.Y))@PAR(PNT(regFile.16!Connections.O1_Trans.X,regFile.16!Connections.O1_Trans.Y))"
       ,to=Const "PAR(PNT(bypass!Connections.I1_Trans.X,bypass!Connections.I1_Trans.Y))@PAR(PNT(bypass!Connections.I1_Trans.X,bypass!Connections.I1_Trans.Y))"}
  ,Edge{edgeID=""
       ,edgeType="idO1.->bypassI2.Trans"
       ,from=Const "PAR(PNT(id.11!Connections.O1_.X,id.11!Connections.O1_.Y))@PAR(PNT(id.11!Connections.O1_.X,id.11!Connections.O1_.Y))"
       ,to=Const "PAR(PNT(bypass!Connections.I2_Trans.X,bypass!Connections.I2_Trans.Y))@PAR(PNT(bypass!Connections.I2_Trans.X,bypass!Connections.I2_Trans.Y))"}
  ,Edge{edgeID="WriteBack.I2"
       ,edgeType="->idI1."
       ,from=Const "4.875 in.@7.25 in."
       ,to=Const "PAR(PNT(id.11!Connections.I1_.X,id.11!Connections.I1_.Y))@PAR(PNT(id.11!Connections.I1_.X,id.11!Connections.I1_.Y))"}
  ,Edge{edgeID=""
       ,edgeType="idO1.->delayI1."
       ,from=Const "PAR(PNT(id.11!Connections.O1_.X,id.11!Connections.O1_.Y))@PAR(PNT(id.11!Connections.O1_.X,id.11!Connections.O1_.Y))"
       ,to=Const "PAR(PNT(delay!Connections.I1_.X,delay!Connections.I1_.Y))@PAR(PNT(delay!Connections.I1_.X,delay!Connections.I1_.Y))"}
  ,Edge{edgeID=""
       ,edgeType="delayO1.->regFileI2.Trans"
       ,from=Const "PAR(PNT(delay!Connections.O1_.X,delay!Connections.O1_.Y))@PAR(PNT(delay!Connections.O1_.X,delay!Connections.O1_.Y))"
       ,to=Const "PAR(PNT(regFile.16!Connections.I2_Trans.X,regFile.16!Connections.I2_Trans.Y))@PAR(PNT(regFile.16!Connections.I2_Trans.X,regFile.16!Connections.I2_Trans.Y))"}]
-}
