module Palette
(
  system
, probe
, clearProbes_MS
, view

, sumInstrs
, sumInstrSS
, sumData

, ilpInstrs
, ilpInstrSS
, ilpData

, no_ilpInstrs
, no_ilpInstrSS
, no_ilpData

, delay
, id
, lastpc
, empty
, mux
, cmux
, rmNops
, reorderBuffer
, execution
, ssBTB
, lastPred
, predict
, predict2
, predict3
, branchTarget
, pipeReg
, regFile
, and_
, or_
, alu
--, findTraps  ------------ do we REALLY need this?  byron
--                           Sat Jan  9 23:08:32 PST 1999
, isBranch
, mispredicted
, okay
, stall
, kill
, bypass
, readWriteHazard
, isLoad
, annotate
, isExecTrans
, modifiesPC
 -- FOR ILD EXAMPLE

, iLen
, decr
, neq0
, true
, false
, decrSel
) where

import System(system)

import qualified Hawk
import qualified Trans
import qualified TransSig as T
import qualified PreludeSig as Signaled

import qualified Utilities
import DLX

import Probe(probe,clearProbes_MS)
import Signal(view)

import qualified PipePredict
import qualified Annotations
import qualified PipeUtils

import Programs

import qualified IFU(ifu)
import qualified RS(rs)
import qualified ROB(rob)
import EUs
import qualified Predict
import qualified TransSig
import qualified Components
-- import Default
import qualified PipeReg




-------------- Simple units


type Signal = Hawk.Signal

-- delay :: Default a => Signal a -> Signal a
delay x = Hawk.del x

true :: Signal Bool
true = Hawk.lift0 True
false :: Signal Bool
false = Hawk.lift0 False

mux :: Signal Bool -> Signal a -> Signal a -> Signal a
mux x y z = Hawk.if' x Hawk.then' y Hawk.else' z

cmux :: Signal (Maybe a) -> Signal a -> Signal a
cmux x y = Hawk.lift2 (\x y -> case x of
                                Just x -> x
                                Nothing -> y
                      ) x y

and_ :: Signal Bool -> Signal Bool -> Signal Bool
and_ x y = Hawk.lift2 (&&) x y
or_ :: Signal Bool -> Signal Bool -> Signal Bool
or_ x y = Hawk.lift2 (||) x y

neq0 :: Num a => Signal a -> Signal Bool
neq0 x = Hawk.lift2 (/=) x (Signal.lift0 0)

decr :: Signal Int -> Signal Int
decr x = x-1


-------------- Pipeline units

type PC = DLXTrans

sumInstrs :: Signal PC -> (Signal DLXTrans,Signal PC)
sumInstrs = Components.fetchDLX (progmem sum')
ilpInstrs :: Signal PC -> (Signal DLXTrans,Signal PC)
ilpInstrs =   Components.fetchDLX (progmem ilp)
no_ilpInstrs :: Signal PC -> (Signal DLXTrans,Signal PC)
no_ilpInstrs =  Components.fetchDLX (progmem no_ilp)


--sumData :: Register a => Signal (Trans a) -> Signal (Trans a)
sumData x = Hawk.mem 4 (datamem sum') x
--ilpData :: Register a => Signal (Trans a) -> Signal (Trans a)
ilpData x = Hawk.mem 4 (datamem ilp) x
--no_ilpData :: Register a => Signal (Trans a) -> Signal (Trans a)
no_ilpData x = Hawk.mem 4 (datamem no_ilp) x


--pipeReg :: Register a => Signal PipeRegCmd -> Signal (Trans a) -> Signal (Trans a)
pipeReg x y = PipeReg.pipeReg Hawk.def x y

type PipeRegCmd = PipeReg.PipeRegCmd

okay :: Signal PipeRegCmd
okay = PipeReg.input
stall :: Signal PipeRegCmd
stall = PipeReg.stall
kill :: Signal PipeRegCmd
kill = PipeReg.kill


branchTargetSS :: Signal [DLXTrans] -> Signal [DLXTrans] -> Signal [DLXTrans]

branchTargetSS x y = PipePredict.btbSS 37 256 x y



ssBTB x y = annotateThingy x (branchTargetSS x y)



annotateThingy x y = Hawk.lift2 (zipWith anno) x y

  where anno x y = let Just (DLX.Reg PC (DLX.Val pc)) = Trans.getDstPC y in
                   if Trans.isBranch x then Trans.addInfo (DLX.Reg SpecPC (DLX.Val $ pc)) x
                   else x


branchTarget :: Signal DLXTrans -> Signal DLXTrans -> Signal DLXTrans
branchTarget x y = PipePredict.btb 37 256 x y

annotate :: Signal DLXTrans -> Signal DLXTrans -> Signal DLXTrans
annotate wire3 wire = mux (isBranch wire) wire7 wire
  where
    wire7 = PipePredict.annotate wire3 wire


--bypass :: Register a => Signal (Trans a) -> Signal (Trans a) -> Signal (Trans a)
bypass x y = TransSig.bypass x (mux (modifiesPC y)
                                    (Signal.lift0 (Trans.nop))
                                    y)

--isBranch :: Register a => Signal (Trans a) -> Signal Bool
isBranch x = TransSig.isBranch x

--isLoad :: Register a => Signal (Trans a) -> Signal Bool
isLoad x = TransSig.isLoad x

--isExecTrans :: Register a => Signal (Trans a) -> Signal Bool
isExecTrans x = PipeUtils.isExecTrans x

--modifiesPC :: Register a => Signal (Trans a) -> Signal Bool
modifiesPC x =  PipeUtils.modifiesPC x

mispredicted :: Signal DLXTrans -> Signal Bool
mispredicted = PipePredict.mispredicted

--readWriteHazard :: Register a => (Signal (Trans a),Signal (Trans a)) -> Signal Bool
readWriteHazard x y = TransSig.rawHazard (x,y)


--findTraps :: Signal DLXTrans -> Signal DLXTrans
--findTraps x = undefined -- Hawk.trunc Utils.isTrapTrans x



regFile :: Signal DLXTrans -> Signal DLXTrans -> Signal DLXTrans
regFile = Components.bypassRF



--alu :: Register a => Signal (Trans a) -> Signal (Trans a)
alu x = Hawk.exec x



----------- Superscalar components

empty :: Signal [a]
empty = Signal.lift0 []

lastpc :: Signal [a] -> Signal a
lastpc = Signaled.last

--rmNops :: Register a => Signal [Trans a] -> Signal [Trans a]
rmNops y = Trans.filterOut Trans.isNop y

predict :: Signal [DLXTrans] -> Signal [DLXTrans]
predict x = Annotations.annotate x

lastPred :: Signal [DLXTrans] -> Signal (Maybe DLXTrans)
lastPred x = map lastPred' x

lastPred' s = sel $ reverse $ justs $
             map  (\instrs -> Trans.getSpecPC instrs)  s
    where
    justs [] = []
    justs (Just x:t) = Just x : justs t
    justs (Nothing:t) = justs t

    sel [] = Nothing
    sel (Just (DLX.Reg SpecPC (DLX.Val x)):t) = Just (Trans.pcTrans x)

predict2 :: Signal [DLXTrans] -> Signal [DLXTrans]
predict2 x = Annotations.annotate2 x

predict3 :: Signal [DLXTrans] -> Signal [DLXTrans]
predict3 x = Annotations.annotate3 x

--reorderBuffer :: Register a => (Signal [Trans a],Signal [Trans (VirtualReg a Int)]) ->
--                  (Signal [Trans a],Signal [Trans (VirtualReg a Int)],Signal Int,Signal Bool)
reorderBuffer x y = ROB.rob 20 (rmNops x,y)

--execution :: Register a => (Signal Bool,Signal [Trans (VirtualReg a Int)]) ->
--                Signal [Trans (VirtualReg a Int)]
execution x y = Hawk.lift1 shuffle (RS.rs (150,execUnits) (x,y))
execUnits :: [EU DLX.DLX_Op (DLX.DLX_Cell DLX.DLXReg DLX.DLX_Word)]
execUnits = [addUnit,addUnit,subUnit,jumpUnit,jumpUnit,multUnit,divUnit]

--shuffle (u:v:w:xs) = w : xs ++ [u,v]
--shuffle [u,v] = [v,u]
shuffle xs = xs

sumInstrSS :: Signal PC  -> Signal Int -> (Signal [DLXTrans],Signal PC )
sumInstrSS =  IFU.ifu (5,progmem sum')  --actually factorial, not sum

ilpInstrSS :: Signal PC -> Signal Int -> (Signal [DLXTrans],Signal PC)
ilpInstrSS = IFU.ifu (5,progmem ilp)
no_ilpInstrSS :: Signal PC -> Signal Int -> (Signal [DLXTrans],Signal PC)
no_ilpInstrSS = IFU.ifu (5,progmem no_ilp)



--------------- ILD-specific components

type Byte = Int
type St = Int


decrSel :: Signal St -> Signal St -> Signal St
decrSel st b = mux (neq0 st) (decr st) (decr b)  -- = decr (mux (neq0 st) st b)

iLen :: Signal Byte -> Signal St
iLen x = x

