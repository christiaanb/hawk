module VisualHawk (new, VisualHawk, getVersionString, genHawk, checkWire, rewriteWith, updateMenu) where

import VisioHawk hiding (index, join, subtract, intersect, union, print, drop, save, remove, run, group, delete, export,select)
import qualified Automation
import qualified Com (putMessage)

import Monad
import LibMonad
import List
import Array
import Maybe
import System
import LibRef

getVersionString :: VisualHawk -> IO String
getVersionString vh = return "1.9.4"

type VisualHawk = RuleDB ObjInfo

type RuleDB a = [(Name, (Drawing a, Drawing a))]

new :: IO VisualHawk
new =
  do{ addButtons
    ; newMenu
    ; db <- mkRuleDB ["RegFile", "RegFileDelay"] -- , "BypassDelay", "ID", "Delay", "HazardDelay", "OrDelay", "Delay3""AndDelay" ]
    ; return db
    }

guardIO :: Bool -> IO ()
guardIO p = if p then return () else ioError (userError "Guard failed")

mkRuleDB :: [Name] -> IO (RuleDB ObjInfo)
mkRuleDB names =
  do{ mapM mkRule names
    }

strToRule :: String -> (Name, Char, Int)
strToRule str =
  ( takeWhile ('-'/=) str
  , head (tail (dropWhile ('-'/=) str))
  , read (drop 2 (dropWhile ('-'/=) str))
  )

sortRule :: (Name, (Drawing ObjInfo, Drawing ObjInfo)) -> (Name, (Drawing ObjInfo, Drawing ObjInfo))
sortRule (name, (a, b)) = if (bounds a) <= (bounds b) then (name, (a, b)) else (name, (b, a))

mkRule :: Name -> IO (Name, (Drawing ObjInfo, Drawing ObjInfo))
mkRule name =
  do{ path       <- getEnv "VISUAL_HAWKROOT"
    ; visio      <- Automation.getActiveObject "Visio.Application"
    ; documents  <- getDocumentsVA visio
    ; rule       <- openVD (path ++ "\\RewriteRules\\" ++ name ++ ".vsd") documents
    ; page       <- itemVP0 (1 :: Int) =<< getPagesVD rule
    ; shapes     <- getShapesVP page
    ; ss         <- (items getCountVS getItemVS) shapes
    ; [lhs, rhs] <- mapM (\s -> mkDrawingShapes =<< getShapesVS s) ss
    ; close rule
    ; return (sortRule (name, (lhs, rhs)))
    } `catch` (\a -> do{ Com.putMessage "Bad transformation."; return ("", (undefined, undefined)) })

--     ; appendFile (path ++ "\\lib\\Gr.hs") (name ++ "lhs = " ++ show lhsG ++ "\n\n" ++ name ++ "rhs = " ++ show rhsG ++ "\n\n")

updateMenu :: VisualHawk -> IO ()
updateMenu vh =
  do{ visio      <- Automation.getActiveObject "Visio.Application"
    ; document   <- getActiveDocumentVA visio
    ; page       <- itemVP0 (1 :: Int) =<< getPagesVD document
    ; shapes     <- getShapesVP page
    ; pageG      <- mkDrawingShapes shapes

    ; window     <- getActiveWindowVA visio
    ; selection  <- getSelectionVW window
    ; selectG    <- mkDrawingSelection selection

    ; let matching = mkMatching pageG selectG vh
    ; addMenu matching
    }

--     ; path       <- getEnv "VISUAL_HAWKROOT"
--     ; appendFile (path ++ "\\lib\\Gr.hs")  
--                  ("select = " ++ show selectG ++ "\n\n" ++ "page = " ++ show pageG ++ "\n\n")
-----------------------------------------------------------
-- Rewrite the current selection

rewriteWith :: String -> VisualHawk -> IO ()
rewriteWith menuStr vh =
  do{ visio      <- Automation.getActiveObject "Visio.Application"
    ; document   <- getActiveDocumentVA visio
    ; page       <- itemVP0 (1 :: Int) =<< getPagesVD document
    ; shapes     <- getShapesVP page
    ; pageG      <- mkDrawingShapes shapes

    ; window     <- getActiveWindowVA visio
    ; selection  <- getSelectionVW window
    ; selectG    <- mkDrawingSelection selection

    ; let (name, dir, num) = strToRule menuStr
    ; let (a,b) = head [ p | (name', p) <- vh, name == name' ]

    ; let (lhsG, rhsG) = if (dir == '>') then (a,b) else (b,a)

    ; let state = (mkMatching' pageG selectG lhsG) !! (num-1)

--    ; Com.putMessage (name ++ ": " ++ show state ++ "\n\n" ++ show (map objName (elems (subDrawing state pageG))))
    ; mapM_ (selectObject shapes window) (map objName (elems (subDrawing state pageG)))
    ; selection  <- getSelectionVW window
    ; selectExtG <- mkDrawingSelection selection
    ; groupVS selection
    ; selection  <- getSelectionVW window
    ; shape      <- itemVS 1 selection
    ; x          <- getFormulaVC =<< getCellsVS "PinX" shape
    ; y          <- getFormulaVC =<< getCellsVS "PinY" shape
    ; deleteVS1 selection
    
    ; path       <- getEnv "VISUAL_HAWKROOT"
    ; documents  <- getDocumentsVA visio
    ; rule       <- openVD (path ++ "\\RewriteRules\\" ++ name ++ ".vsd") documents
    ; page       <- itemVP0 (1 :: Int) =<< getPagesVD rule
    ; shapes     <- getShapesVP page
    ; [lhs, rhs] <- (items getCountVS getItemVS) shapes
    ; lhsI       <- getCountVS =<< getShapesVS lhs
    ; rhsI       <- getCountVS =<< getShapesVS rhs
    ; if lhsI <= rhsI then
         if (dir == '>') then copy rhs else copy lhs
       else
         if (dir == '>') then copy lhs else copy rhs
    ; close rule

    ; pasteVW window
    ; selection <- getSelectionVW =<< getActiveWindowVA visio
    ; shape <- itemVS 1 selection
    ; setFormulaVC x =<< getCellsVS "PinX" shape
    ; setFormulaVC y =<< getCellsVS "PinY" shape
    ; ungroupVS selection

    ; let stateA = head (subst selectExtG lhsG)
--     ; Com.putMessage ("lhsG = " ++ show lhsG ++ "\n\n" ++ "selectExtG = " ++ show selectExtG ++ "\n\n" ++ "stateA = " ++ show stateA ++ "\n\n")
    ; let translate = walk lhsG selectExtG stateA
    ; selection  <- getSelectionVW =<< getActiveWindowVA visio
    ; ss         <- items countVS itemVS selection
    ; mapM_ (updateObject translate) ss
    
    }

type Table = [(String, (Object ObjInfo, ((String, String), (String, String))))]

walk :: Drawing ObjInfo -> Drawing ObjInfo -> State -> Table
walk lG sG [] = []
walk lG sG ((s := l):ss)
  | null (objData1 (lG!l)) = walk lG sG ss
  | otherwise              = ( objData1 (lG!l), (lG!l, loc (objInfo (sG!s))) ) : walk lG sG ss

updateObject :: Table -> IVShape () -> IO ()
updateObject translate shape =
  do{ d1    <- getData1VS shape
    ; case (lookup d1 translate) of
        Nothing -> return ()
        (Just (obj, ((fx, fy), (tx, ty)))) -> 
            do{ case (objFrom obj) of
                  None _ -> do{ setFormulaVC fx =<< getCellsVS "BeginX" shape
                              ; setFormulaVC fy =<< getCellsVS "BeginY" shape}
                  _ -> return ()
              ; case (objTo obj) of
                  None _ -> do{ setFormulaVC tx =<< getCellsVS "EndX" shape
                              ; setFormulaVC ty =<< getCellsVS "EndY" shape }
                  _ -> return ()
              }
    }

selectObject :: IVShapes () -> IVWindow () -> Name -> IO ()
selectObject shapes window name =
  do{ shape <- getItemVS name shapes
    ; VisioHawk.select shape (fromEnum VisSelect) window
    }

getGraph :: IVApplication () -> IVShape () -> IO (Drawing ObjInfo)
getGraph visio grp =
  do{ ungroup grp
    ; window'    <- getActiveWindowVA visio
    ; selection' <- getSelectionVW window'
    ; mkDrawingSelection selection'
    }

-----------------------------------------------------------
-- Newer Graphs

data Object a = Object {objType :: ObjType, objFrom, objTo :: Gptr, objInfo :: a} deriving Show

data ObjInfo = ObjInfo {name :: Name, data1 :: String, loc :: ((String, String), (String, String))} deriving Show

type Drawing a = Array Int (Object a)

type ObjType = String

data Gptr = None String
          | Gptr Name Port deriving (Show, Eq)

gptrNameEq :: Gptr -> Gptr -> Bool
gptrNameEq (Gptr n p) (Gptr n' p') = n == n'
gptrNameEq (None n) (None n') = n == n'
gptrNameEq _ _ = False

gptrPortEq :: Gptr -> Gptr -> Bool
gptrPortEq (Gptr n p) (Gptr n' p') = p == p'
gptrPortEq _ _ = True

-- type Name = String
type Port = String

subst :: Drawing a -> Drawing a -> [State]
subst d1 d2 = search bt (mkIsoCSP d1 d2)

mkMatching :: Drawing ObjInfo -> Drawing ObjInfo -> RuleDB ObjInfo -> [Name]
mkMatching page select [] = []
mkMatching page select ((n, (lhs, rhs)):rs) =
  f lhs (n ++ "->") ++ f rhs (n ++ "-<") ++ mkMatching page select rs
  where f pat name = zipWith (\i s -> name ++ show i) [1..] (mkMatching' page select pat)

mkMatching' :: Drawing ObjInfo -> Drawing ObjInfo -> Drawing ObjInfo -> [State]
mkMatching' page select pat =
  [ m |  match select pat, m <- subst pat page, matchName select (subDrawing m page) ]

match :: Drawing a -> Drawing a -> Bool
match d1 d2 = not (null (subst d1 d2))

matchName :: Drawing ObjInfo -> Drawing ObjInfo -> Bool
matchName d1 d2 = not (null (search bt (mkNameCSP d1 d2)))

mkIsoCSP :: Drawing a -> Drawing a -> CSP
mkIsoCSP d1 d2 = CSP i j (cmp d1 d2)
  where (_, i) = bounds d1
        (_, j) = bounds d2

mkNameCSP :: Drawing ObjInfo -> Drawing ObjInfo -> CSP
mkNameCSP d1 d2 = CSP i j (cmpName d1 d2)
  where (_, i) = bounds d1
        (_, j) = bounds d2

subDrawing :: State -> Drawing a -> Drawing a
subDrawing is d = listArray (1, length os) os
  where os = [ d!i | (_ := i) <- is ]

cmpName :: Drawing ObjInfo -> Drawing ObjInfo -> Assign -> Assign -> Bool
cmpName pat gra (a := x) (b := y) =
  objName (pat!a) == objName (gra!x) &&
  objName (pat!b) == objName (gra!y)

objName :: Object ObjInfo -> Name
objName obj = name (objInfo obj)

objData1 :: Object ObjInfo -> String
objData1 obj = data1 (objInfo obj)


cmp :: Drawing a -> Drawing a -> Assign -> Assign -> Bool
cmp pat gra (a := x) (b := y)
 | a == b    = x == y && (objType a' == objType x')
 | otherwise =
  (x /= y || a == b)
  && (objType a' == objType x') && (objType b' == objType y')
  && (gptrNameEq (objFrom a') (objFrom b')) == (gptrNameEq (objFrom x') (objFrom y'))
  && (gptrNameEq (objTo a')   (objTo b'))   == (gptrNameEq (objTo x')   (objTo y'))
  && (gptrNameEq (objFrom a') (objTo b'))   == (gptrNameEq (objFrom x') (objTo y'))
  && (gptrNameEq (objTo a')   (objFrom b')) == (gptrNameEq (objTo x')   (objFrom y'))
  && (gptrPortEq (objTo a') (objTo x')) && (gptrPortEq (objFrom a') (objFrom x'))
  && (gptrPortEq (objTo b') (objTo y')) && (gptrPortEq (objFrom b') (objFrom y'))
  where
    [a', b', x', y'] = [pat!a, pat!b, gra!x, gra!y]

mkDrawingSelection :: IVSelection () -> IO (Drawing ObjInfo)
mkDrawingSelection select =
  do{ os <- mapM mkObject =<< (items countVS itemVS) select
    ; return (listArray (1, length os) os)
    }

mkDrawingShapes :: IVShapes () -> IO (Drawing ObjInfo)
mkDrawingShapes shapes =
  do{ os <- mapM mkObject =<< (items getCountVS getItemVS) shapes
    ; return (listArray (1, length os) os)
    }

mkObject :: IVShape () -> IO (Object ObjInfo)
mkObject shape =
  do{ oneD  <- getOneDVS shape
    ; let mkObj = if (oneD /= 0) then mkWire else mkUnit
    ; mkObj shape
    }

mkWire :: IVShape () -> IO (Object ObjInfo)
mkWire shape =
  do{ name  <- getNameVS shape
    ; data1 <- getData1VS shape
    ; fromX <- getFormulaVC =<< getCellsVS "BeginX" shape
    ; fromY <- getFormulaVC =<< getCellsVS "BeginX" shape
    ; toX   <- getFormulaVC =<< getCellsVS "EndX" shape
    ; toY   <- getFormulaVC =<< getCellsVS "EndX" shape
    ; return Object { objType = "!Wire!", objFrom = parseGptr data1 fromX, objTo = parseGptr data1 toX
                    , objInfo = ObjInfo { name = name, data1 = data1, loc = ((fromX, fromY), (toX, toY))}}
    } 

mkUnit :: IVShape () -> IO (Object ObjInfo)
mkUnit shape =
  do{ name <- getNameVS shape
    ; typ  <- getData1VS shape
    ; return Object { objType = typ, objFrom = Gptr name "", objTo = Gptr name ""
                    , objInfo = ObjInfo { name = name, data1 = "", loc = (("", ""), ("", ""))}}
    } 

-- "PAR(PNT(name!Connections.I1_SignalInt.X,name!Connections.I1_SignalInt.Y))"
-- or "1.23in."

parseGptr :: String -> String -> Gptr
parseGptr d1 s
  | isPrefixOf "PAR(PNT(" s =
     let name = takeWhile ('!'/=) (drop 8 s)
         port = takeWhile ('.'/=) $ drop 13 $ dropWhile ('!'/=) s
      in Gptr name port
  | otherwise = None d1


-----------------------------------------------------------
-- Generate a Hawk program from the current Visio drawing

genHawk :: FilePath -> Bool -> VisualHawk -> IO ()
genHawk name probes vh =
  do{ visio  <- Automation.getActiveObject "Visio.Application"
    ; page   <- getActivePageVA visio
    ; shapes <- getShapesVP page
    ; ss     <- (items getCountVS getItemVS) shapes
    ; us     <- filterShapes unit ss
    ; as     <- filterShapes arrow ss
    ; units  <- mapl getUnit us
    ; arrows <- mapl getArrow as
    ; writeFile name (buildHawkFun probes units arrows)
    ; return ()
    } `catch` (\a -> return ())

-----------------------------------------------------------
-- Check a wire for type and direction

checkWire :: VisualHawk -> IO ()
checkWire vh =
  do{ visio     <- Automation.getActiveObject "Visio.Application"
    ; window    <- getActiveWindowVA visio
    ; selection <- getSelectionVW window
    ; count     <- countVS selection
    ; if (count /= 1) then
        return ()
       else
        do{ wire <- itemVS 1 selection
          ; oneD <- getOneDVS wire
          ; if (oneD == 0) then
              return ()
             else
              do{ arrow     <- getArrow wire
                ; bumpArrow (okArrowTo arrow)   wire ("EndX"  , "EndY")
                ; arrow     <- getArrow wire
                ; bumpArrow (okArrowFrom arrow) wire ("BeginX", "BeginY")
                }
          }
    } `catch` (\a -> return ())

bumpArrow :: Bool -> IVShape a -> (String, String) -> IO ()
bumpArrow False shape (f1, f2) =
  do{ bumpFormula shape f1
    ; bumpFormula shape f2
    }
bumpArrow True _ _ = return ()

bumpFormula :: IVShape a -> String -> IO ()
bumpFormula shape name =
  do{ cell <- getCellsVS name shape
    ; val <- getResultIUVC cell
    ; setResultIUVC (val + 0.25) cell
    }

-----------------------------------------------------------
-- Add a Toolbar button

addButtons :: IO ()
addButtons =
  do{ visio        <- Automation.getActiveObject "Visio.Application"
    ; ui           <- getBuiltInToolbarsVA 0 visio
    ; toolbarSets  <- getToolbarSetsVUIO ui
    ; toolbarSet   <- getItemAtIDVTS (fromEnum VisUIObjSetDrawing) toolbarSets
    ; toolbars     <- getToolbarsVTS toolbarSet
    ; toolbar      <- getItemVT 2 toolbars
    ; toolbarItems <- getToolbarItemsVT toolbar

    ; addButton ("View Text Model", "ShowInMenu.GenHawk"   , "view.ico")     toolbarItems
    ; addButton ("Execute Model"  , "ShowInMenu.ExecHawk"  , "exec.ico")     toolbarItems
--     ; addButton ("RewriteRegFile" , "ShowInMenu.RewriteRegFile", "unabst.ico")   toolbarItems
--     ; addButton ("RewriteDelay"   , "ShowInMenu.RewriteDelay"  , "abstract.ico") toolbarItems
--     ; addButton ("Unabstract"     , "ShowInMenu.DeAbstract", "unabst.ico")   toolbarItems
--     ; addButton ("Abstract"       , "ShowInMenu.Abstract"  , "abstract.ico") toolbarItems

    ; setCustomToolbarsVA ui visio
    }

iconPath = "C:\\hawk\\visual_hawk\\lib\\Icons\\"

addButton :: (String, String, String) -> IVToolbarItems () -> IO ()
addButton (action, macro, icon) toolbar =
  do{ new <- addAtVTI 0 toolbar
    ; setActionTextVTI action new
    ; setAddOnNameVTI  macro  new
    ; iconFileName     (iconPath ++ icon)           new
    ; setCntrlTypeVTI  (fromEnum VisCtrlTypeBUTTON) new
    ; setPriorityVTI   1      new
    }

-----------------------------------------------------------
-- Add a Menu

newMenu :: IO ()
newMenu =
  do{ visio        <- Automation.getActiveObject "Visio.Application"
    ; ui           <- getBuiltInMenusVA visio
    ; menuSets     <- getMenuSetsVUIO ui
    ; menuSet      <- getItemAtIDVMS (fromEnum VisUIObjSetDrawing) menuSets
    ; menus        <- getMenusVMS menuSet
    ; menuObj      <- addAt 7 menus
    ; setCaptionVM "T&ransformations" menuObj
    ; menu         <- getMenuItemsVM menuObj

    ; addMenuItem ("&Update transformations", "ShowInMenu.UpdateMenu", "Find applicable rewrites") False menu
    ; addMenuItem ("", "", "") True menu

    ; setCustomMenusVA ui visio
    }

addMenu :: [String] -> IO ()
addMenu rules =
  do{ visio        <- Automation.getActiveObject "Visio.Application"
    ; ui           <- getCustomMenusVA visio
    ; menuSets     <- getMenuSetsVUIO ui
    ; menuSet      <- getItemAtIDVMS (fromEnum VisUIObjSetDrawing) menuSets
    ; menus        <- getMenusVMS menuSet
    ; menuObj      <- getItemVM 7 menus
    ; menu         <- getMenuItemsVM menuObj

    ; i            <- getCountVMI menu		-- Clear all the old transformations
    ; mapM_ (\n -> deleteVMI =<< getItemVMI n menu) (reverse [2..i-1])

    ; mapM_ (flip addRewriteMenuItem menu) rules

    ; setCustomMenusVA ui visio
    }

addRewriteMenuItem :: String -> IVMenuItems () -> IO ()
addRewriteMenuItem rule = addMenuItem (rule, "ShowInMenu.ReWriteWith \"" ++ rule ++ "\"", "Applies rewrite " ++ rule) False

addMenuItem :: (String, String, String) -> Bool -> IVMenuItems () -> IO ()
addMenuItem (name, fun, help) sep menuItems =
  do{ menuItem <- addVMI menuItems
    ; setCaptionVMI    name menuItem
    ; setAddOnNameVMI  fun  menuItem
    ; setMiniHelpVMI   help menuItem
    ; if sep then setCmdNumVMI 0 menuItem else return ()
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
  do{ name <- getNameVS shape
    ; code <- getData1VS shape
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
          | In  Int deriving (Eq, Show, Ord)

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
-- underscore followed by nothing is a polymorphic port.

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
  do{ cell <- getCellsVS cellName shape
    ; name <- getFormulaVC cell
    ; return (parseTPort name)
    }

-----------------------------------------------------------
-- type Arrow

type Arrow = (Name, (Maybe TPort, Maybe TPort))  -- From, To

getArrow :: IVShape a -> IO Arrow
getArrow shape =
  do{ name <- getNameVS shape
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
pArrow (n, ps) = mapPair (fmap (flip (,) n)) ps

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

buildHawkFun :: Bool -> [Unit] -> [Arrow] -> String
buildHawkFun probed us as =
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
  do{ oneD <- getOneDVS shape
    ; code <- getData1VS shape
    ; return (do{ guard (oneD == 0 && code /= ""); return shape})
    }

arrow :: IVShape a -> IO (Maybe (IVShape a))
arrow shape =
  do{ oneD <- getOneDVS shape
    ; return (do{ guard (oneD /= 0); return shape})
    }

-----------------------------------------------------------
-- Misc Utilities

items :: Monad a => (c -> a Int) -> (Int -> c -> a d) -> c -> a [d]
items getCount getItem enum =
  do{ n <- getCount enum
    ; mapM (\i -> getItem i enum) [1..n]
    }

findl :: Eq a => a -> [(a, [b])] -> [b]
findl k l = maybe [] id (lookup k l)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

-----------------------------------------------------------
-- Daans code for callbacks, needs fixing up in Haskell Script

{-
   page # onEvent_6_0 "VisEventProc" 
     (foo :: Variant a => Int -> IUnknown () -> Int -> Int -> IUnknown () -> a -> IO ()) 
foo :: IDispatch () -> IO ()
foo _ =  Com.putMessage "shape added"

  do{ onEvent_1_0 "ShapeAdded" (foo) visio
-}

-----------------------------------------------------------
-- Some CSP code :-)

type Var = Int
type Value = Int

data Assign = Var := Value deriving (Eq, Ord, Show)

type Relation = Assign -> Assign -> Bool

data CSP = CSP { vars, vals :: Int, rel :: Relation } 

type State = [Assign]

level :: Assign -> Var
level (l := v) = l

currentLevel :: State -> Var
currentLevel [] = 0
currentLevel ((l := v):_) = l

complete :: CSP -> State -> Bool
complete CSP{vars} s = currentLevel s == vars

data Tree a = Node { label :: a, children :: [Tree a] }

type Transform a b = Tree a -> Tree b

mapTree  :: (a -> b) -> Transform a b
mapTree f (Node a cs) = Node (f a) (map (mapTree f) cs)

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a cs) = f a (map (foldTree f) cs)

filterTree :: (a -> Bool) -> Transform a a
filterTree p = foldTree f
  where f a cs = Node a (filter (p . label) cs)

prune :: (a -> Bool) -> Transform a a
prune p = filterTree (not . p)

leaves :: Tree a -> [a]
leaves (Node l []) = [l]
leaves (Node _ cs) = concat (map leaves cs)

initTree :: (a -> [a]) -> a -> Tree a
initTree f a = Node a (map (initTree f) (f a))

mkTree :: CSP -> Tree State
mkTree CSP{vars,vals,rel} = initTree next []
  where next ss = [ s:ss | currentLevel ss < vars, j <- [1..vals], let s = (currentLevel ss + 1) := j, rel s s ]

earliestInconsistency :: CSP -> State -> Maybe (Var,Var)
earliestInconsistency CSP{rel} [] = Nothing
earliestInconsistency CSP{rel} (a:as) = 
	case filter (not . rel a) (reverse as) of
          [] -> Nothing
          b:_ -> Just (level a, level b)

type ConflictSet = Maybe [Var]

knownConflict :: ConflictSet -> Bool
knownConflict (Just (a:as)) = True
knownConflict _             = False

knownSolution :: ConflictSet -> Bool
knownSolution (Just []) = True
knownSolution _         = False

type Labeler = CSP -> Transform State (State, ConflictSet)

search :: Labeler -> CSP -> [State]
search labeler csp =
  (map fst . filter (knownSolution . snd) . leaves . prune (knownConflict . snd) . labeler csp . mkTree) csp

inconsistencyConflicts :: CSP -> State -> ConflictSet
inconsistencyConflicts csp s =
  case earliestInconsistency csp s of
    Nothing    -> if complete csp s then Just [] else Nothing
    Just (a,b) -> Just [a,b]

bt :: Labeler
bt csp = mapTree f
      where f s = (s, inconsistencyConflicts csp s)
{-
  do{ visio      <- Automation.getActiveObject "Visio.Application"
    ; window     <- getActiveWindowVA visio
    ; selection  <- getSelectionVW window
    ; grold      <- mkDrawing selection
    ; groupVS selection
    ; selection  <- getSelectionVW window
    ; shape      <- itemVS 1 selection
    ; x          <- getFormulaVC =<< getCellsVS "PinX" shape
    ; y          <- getFormulaVC =<< getCellsVS "PinY" shape
    ; ungroupVS selection
    ; selection  <- getSelectionVW window

    ; documents  <- getDocumentsVA visio
    ; rule       <- openVD ruleName documents
    ; pages      <- getPagesVD rule
    ; page       <- itemVP0 (1 :: Int) pages
    ; shapes     <- getShapesVP page
    ; [lhs, rhs] <- (items getCountVS getItemVS) shapes
    ; copy lhs

    ; grlhs <- getGraph visio lhs
    ; grrhs <- getGraph visio rhs
    ; writeFile "C:\\hawk\\visual_hawk\\lib\\test.hs" ("gold = " ++ show grold ++ "\n\nglhs = " ++ show grlhs ++ "\n\ngrhs = " ++ show grrhs ++ "\n\n")
    
--    ; guardIO (matchG grlhs grold || matchG grrhs grold)
    
    ; pasteVW window
    ; close rule
    ; deleteVS1 selection

    ; selection <- getSelectionVW =<< getActiveWindowVA visio
    ; shape <- itemVS 1 selection
    ; setFormulaVC x =<< getCellsVS "PinX" shape
    ; setFormulaVC y =<< getCellsVS "PinY" shape

--    ; mapM_ (setWire shape) (unifyWires grfrom grold)
    } `catch` (\a -> do{ Com.putMessage "Error in rewrite." })
-}

