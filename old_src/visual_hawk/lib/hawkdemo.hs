module HawkDemo (genHawk, genHawkProbed, checkWire) where

import HScript
import VisioHawk hiding (addRef, release, index, subtract, print, drop, getUserName, save, setDescription, getDescription, getFullName, remove, getObject, run, group, union, intersect, delete, export, export0, export1, export2, export3, export4)

import LibMonad
import List
import Maybe
import System

-----------------------------------------------------------
-- Generate a Hawk program from the current Visio drawing

genHawk :: Export
genHawk = exportRes1 inEmpty resString (genHawk' False)

genHawkProbed :: Export
genHawkProbed = exportRes1 inEmpty resString (genHawk' True)

genHawk' :: Bool -> String -> IO ()
genHawk' probes path =
  do{ visio' <- getActiveObject "Visio.Application"
    ; visio  <- getApplication visio'
    ; page   <- getActivePage visio
    ; shapes <- getShapes0 page
    ; ss     <- (items getCount4 getItem4) shapes
    ; us     <- filterShapes unit ss
    ; as     <- filterShapes arrow ss
    ; units  <- mapl getUnit us
    ; arrows <- mapl getArrow as
    ; let hawk' = buildHawkFun probes path units arrows
    ; writeFile (path ++ "CPU.hs") hawk'
    ; return ()
    } `catch` (\a -> return ())

bfoo :: IO ()
bfoo  =
  do  visio' <- getActiveObject "Visio.Application"
      visio  <- getApplication visio'
      p <- getActivePage visio
      w <- getActiveWindow visio
      copy2 w
      sel <- getSelection w
      item <- getItem3 1 sel
      pinx <- getCellFormula "PinX" item
      piny <- getCellFormula "PinY" item
   ---   print pinx
   ---
      s <- drawRectangle0 1 1 1 1 p
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

setCells s st val =
  do c <- getCells st s
     setFormula val c

getCellFormula s i =
  do x <-  getCells s i
     getFormula x


-----------------------------------------------------------
-- Check a wire for type and direction

checkWire :: Export
checkWire = exportRes0 inEmpty checkWire'

checkWire' :: IO ()
checkWire' =
  do{ visio'    <- getActiveObject "Visio.Application"
    ; visio     <- getApplication visio'
    ; window    <- getActiveWindow visio
    ; selection <- getSelection window
    ; wire      <- getItem3 1 selection
    ; arrow     <- getArrow wire
    ; bumpArrow (okArrowTo arrow)   wire ("EndX"  , "EndY")
    ; arrow     <- getArrow wire
    ; bumpArrow (okArrowFrom arrow) wire ("BeginX", "BeginY")
    } `catch` (\a -> return ())

bumpArrow :: Bool -> IVShape a -> (String, String) -> IO ()
bumpArrow False shape (f1, f2) =
  do{ bumpFormula shape f1
    ; bumpFormula shape f2
    }
bumpArrow True _ _ = return ()

bumpFormula :: IVShape a -> String -> IO ()
bumpFormula shape name =
  do{ cell <- getCells name shape
    ; val <- getResultIU cell
    ; setResultIU (val + 0.25) cell
    }

-----------------------------------------------------------
-- type Name

type Name = String

-----------------------------------------------------------
-- type Unit

type Code = String
type Unit = (Name, Code)

getUnit :: IVShape a -> IO Unit
getUnit shape =
  do{ name <- getName3 shape
    ; code <- getData1 shape
    ; return (name, code)
    }

-----------------------------------------------------------
-- type Type (Could be expanded into a complete type checker)
           -- But that is left as an excercise for the reader

type Type = String

  -- Primitive(!) type checking
okTypes :: Type -> Type -> Bool
okTypes t1 t2 = null t1 || null t2 || t1 == t2

-----------------------------------------------------------
-- type TPort

data Info = Out Int
          | In Int deriving (Eq, Show, Ord)

type TPort = (Name, Info, Type)

typeOfPort :: TPort -> Type
typeOfPort (_, _, t) = t

infoOfPort :: TPort -> Info
infoOfPort (_, i, _) = i

nameOfPort :: TPort -> Name
nameOfPort (n, _, _) = n

inPort :: TPort -> Bool
inPort (_, In _, _) = True
inPort _ = False

outPort :: TPort -> Bool
outPort (_, Out _, _) = True
outPort _ = False

-- "PAR(PNT(name!Connections.I1_SignalInt.X,name!Connections.I1_SignalInt.Y))"
-- or "1.23in."

-- The type of the port is  either I(n) or O(ut) and a positional number
-- It is followed by a underscore and then the type of the port,
-- underscore followed by nothig is a polymorphic port.

-- ex: O1_Int, I2_ or O1_ListInt

parseTPort :: String -> Maybe TPort
parseTPort cs
  | head cs == 'P' =
     let name = takeWhile ('!'/=) (drop 8 cs)
         port = drop 13 $ dropWhile ('!'/=) cs
         i    = takeWhile ('_'/=) port
         ttyp = takeWhile ('.'/=) (tail $ dropWhile ('_'/=) port)
         info (d:ns)
           | d == 'i' || d == 'I' = In (read ns)
           | otherwise            = Out (read ns)
      in Just (name, info i, ttyp)
  | otherwise = Nothing

getTPort :: IVShape a -> String -> IO (Maybe TPort)
getTPort shape cellName =
  do{ cell <- getCells cellName shape
    ; name <- getFormula cell
    ; return (parseTPort name)
    }

-----------------------------------------------------------
-- type Arrow

type Arrow = (Name, (Maybe TPort, Maybe TPort))  -- From, To

getArrow :: IVShape a -> IO Arrow
getArrow shape =
  do{ name <- getName3 shape
    ; p1 <- getTPort shape "BeginX"
    ; p2 <- getTPort shape "EndX"
    ; return (name, (p1, p2))
    }

okArrowType :: Arrow -> Bool
okArrowType (_, (Just p1, Just p2)) =
  okTypes (typeOfPort p1) (typeOfPort p2)
okArrowType _ = True

okArrowFrom :: Arrow -> Bool
okArrowFrom a@(_, (Just p1, _)) =
  okArrowType a
  && outPort p1
okArrowFrom _ = True

okArrowTo :: Arrow -> Bool
okArrowTo a@(_, (_, Just p2)) =
  okArrowType a
  && inPort p2
okArrowTo _ = True

portsArrow :: Arrow -> Maybe ((TPort, Name), (TPort, Name))
portsArrow (n, (Just p1, Just p2)) = Just ((p1, n), (p2, n))
portsArrow _ = Nothing

pArrow :: Arrow -> (Maybe (TPort, Name), Maybe (TPort, Name))
pArrow (n, ps) = mapPair (map (flip (,) n)) ps

-----------------------------------------------------------
-- Build Hawk Program

type IOS = (Name, [[Name]])     -- Uni, [ports, potentially w/ multiple names]

buildHawk :: [Arrow] -> ([IOS], [IOS], ([Name], [Name]))
buildHawk as = (f os, f is, (nis \\ nos, nos \\ nis))
  where (os, is) = mapPair catMaybes (unzip (map pArrow as))
        (nos, nis) = mapPair (map snd) (os, is)
        f ps = map buildUnit (groupBy sameUnit (sort ps))
        sameUnit (p1, n1) (p2, n2) = nameOfPort p1 == nameOfPort p2

buildUnit :: [(TPort, Name)] -> (Name, [[Name]])
buildUnit ps = ((nameOfPort . fst . head) ps, map (map snd) gps)
 where gps = (groupBy samePort ps)
       samePort (p1, n1) (p2, n2) = infoOfPort p1 == infoOfPort p2

buildHawkFun :: Bool -> String -> [Unit] -> [Arrow] -> String
buildHawkFun probed path us as =
    prelude ++
    "cpu " ++ ppmis ++ " = " ++ ppmos ++ "\n" ++
    "  where" ++
    concatMap ("\n    "++) (map ppBind bs) ++ "\n\n" ++
    postlude

  where (os, is, (mis, mos)) = buildHawk as
        bs = sortBy cmpBindName $ filter (not . null . fst3) (map mkBind us)
        mkBind (n, c) = (findl n os, (c, n), map (mkProbe . head) (findl n is))

        ppmis = ppProd (map fixName (sort mis))
        ppmos = ppProd (map mkProbe (sort mos))

        prelude =
          "module Main where\n" ++
          "import Palette\n\n" 

        postlude =
          "main =\n" ++
          "  do{ clearProbes_MS\n" ++
          "    ; putStr (show $ take 100 $ view $ cpu ())\n" ++
          "    }\n"

        mkProbe n
          | probed    = parens ("probe \"" ++ n ++ "\" " ++ fixName n)
          | otherwise = fixName n

fixName :: Name -> Name
fixName [] = []
fixName (c:cs) = stripDot (toLower c : cs)


-----------------------------------------------------------
-- Pretty Printing Utilities

type Bind = ([[Name]], (Code, Name), [Name])               -- Out, function, In

cmpBindName :: Bind -> Bind -> Ordering
cmpBindName a b = compare (snd3 a) (snd3 b)

ppBind (oss, (c, n), is) =
  ppProd (map ppRes oss) ++ " = " ++ c ++ " " ++ space " " is
  --  ++ "\t\t-- " ++ n

ppProd [n] = n
ppProd ns = parens (space ", " ns)

ppRes ns = space "@" (map fixName ns)

space s = concat . intersperse s

stripDot :: String -> String
stripDot = filter (/='.')

parens :: String -> String
parens x = "(" ++ x ++ ")"

quote :: String -> String
quote "" = ""
quote (c:cs)
  | c == '\\'  = '\\':'\\':quote cs
  | otherwise = c:quote cs


-----------------------------------------------------------
-- Utilities on IVShapes

filterShapes :: (IVShape a -> IO (Maybe (IVShape a))) -> [IVShape a] -> IO [IVShape a]
filterShapes f ss =
  do{ ss' <- mapl f ss
    ; return (catMaybes ss')
    }

unit :: IVShape a -> IO (Maybe (IVShape a))
unit shape =
  do{ oneD <- getOneD0 shape
    ; code <- getData1 shape
    ; return (do{ guard (oneD == 0 && code /= ""); return shape})
    }

arrow :: IVShape a -> IO (Maybe (IVShape a))
arrow shape =
  do{ oneD <- getOneD0 shape
    ; return (do{ guard (oneD /= 0); return shape})
    }

-----------------------------------------------------------
-- Misc Utilities

items c f = \enum ->
  do{ n <- c enum
    ; mapM (\i -> do{ f i enum}) [1..n]
    }

findl k l = maybe [] id (lookup k l)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)
