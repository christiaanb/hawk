module Palette
(
  system
, progmem
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
, symb_Instr

, rawHazSymInstr
, rawHazSymData

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
, bypassDstMany
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

---------------------------------------------------
-- Columbia Example --
-- , module OA64_Demo
---------------------------------------------------
-- Symbolic p6 Example --
, module Symbp6Demo
, module RawHaz

) where

-- import OA64_Demo
import Symbp6Demo
import RawHaz
import System(system)
import qualified Hawk
import qualified Trans
import qualified TransSig as T
import qualified PreludeSig as Signaled

import qualified Utilities
import DLX
import Symbo


import Probe(probe,clearProbes_MS)
import Signal(view)

import qualified PipePredict
--import qualified Annotations
import qualified PipeUtils

--import Programs

import qualified IFU(ifu)
import qualified RS(rs)
import qualified ROB(rob)
import EUs
import qualified Predict
import qualified TransSig
import qualified Components
-- import Default
import qualified PipeReg
import Words
import VRegister




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

--decr :: Signal Int -> Signal Int
decr x = x-1


-------------- Pipeline units

type PC = DLX_Trans Int

type DLXTrans = DLX_Trans Int

sumInstrs :: Signal PC -> (Signal DLXTrans,Signal PC)
sumInstrs = Components.fetchDLX (progmem sum')
ilpInstrs :: Signal PC -> (Signal DLXTrans,Signal PC)
ilpInstrs =   Components.fetchDLX (progmem ilp)
no_ilpInstrs :: Signal PC -> (Signal DLXTrans,Signal PC)
no_ilpInstrs =  Components.fetchDLX (progmem no_ilp)

-- symbolic read-after-write hazard program for pipe2, NAD 28 Jan 1999
rawHazSymInstr :: Signal (DLX_Trans (Symbo Int)) -> (Signal (DLX_Trans (Symbo Int)),Signal (DLX_Trans (Symbo Int)))
rawHazSymInstr = Components.fetchDLX (progmem raw_haz)
rawHazSymData x = Hawk.mem 4 (datamem raw_haz) x


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


branchTargetSS :: Word a => Signal [DLX_Trans a] -> Signal [DLX_Trans a] -> Signal [DLX_Trans a]

branchTargetSS x y = PipePredict.btbSS 37 256 x y


-- 29 Jan 1999, Byron, Nancy, and Jeff, btb eliminates any transactions
-- after a branch in its output because it is going to make a predication
-- and its prediction may have already been fetched in this packet
ssBTB x y = Hawk.lift1 foo $ annotateThingy x (branchTargetSS x y)
  where foo [] = []
        foo (t:ts) = if Trans.isBranch t then [t]
                     else t:foo ts

annotateThingy x y = Hawk.lift2 (zipWith anno) x y

  where anno x y = let Just (DLX.Reg PC (DLX.Val pc)) = Trans.getDstPC y in
                   if Trans.isBranch x then Trans.addInfo (DLX.Reg SpecPC (DLX.Val $ pc)) x
                   else x


branchTarget :: Word a => Signal (DLX_Trans a) -> Signal (DLX_Trans a) -> Signal (DLX_Trans a)
branchTarget x y = PipePredict.btb 37 256 x y

annotate :: Word a => Signal (DLX_Trans a) -> Signal (DLX_Trans a) -> Signal (DLX_Trans a)
annotate wire3 wire = mux (isBranch wire) wire7 wire
  where
    wire7 = PipePredict.annotate wire3 wire


--bypass :: Register a => Signal (Trans a) -> Signal (Trans a) -> Signal (Trans a)
bypass x y = TransSig.bypass x (mux (modifiesPC y)
                                    (Signal.lift0 (Trans.nop))
                                    y)
bypassDstMany x = TransSig.bypassDstMany x

--isBranch :: Register a => Signal (Trans a) -> Signal Bool
isBranch x = TransSig.isBranch x

--isLoad :: Register a => Signal (Trans a) -> Signal Bool
isLoad x = TransSig.isLoad x

--isExecTrans :: Register a => Signal (Trans a) -> Signal Bool
isExecTrans x = PipeUtils.isExecTrans x

--modifiesPC :: Register a => Signal (Trans a) -> Signal Bool
modifiesPC x =  PipeUtils.modifiesPC x

mispredicted :: Word a => Signal (DLX_Trans a) -> Signal Bool
mispredicted = PipePredict.mispredicted

--readWriteHazard :: Register a => (Signal (Trans a),Signal (Trans a)) -> Signal Bool
readWriteHazard x y = TransSig.rawHazard (x,y)


--findTraps :: Signal DLXTrans -> Signal DLXTrans
--findTraps x = undefined -- Hawk.trunc Utils.isTrapTrans x



regFile :: Word a => Signal (DLX_Trans a) -> Signal (DLX_Trans a) -> Signal (DLX_Trans a)
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

predict :: Word a => Signal [DLX_Trans a] -> Signal [DLX_Trans a]
--predict x = annotate x

lastPred :: Word a => Signal [DLX_Trans a] -> Signal (Maybe (DLX_Trans a))
lastPred x = fmap lastPred' x

lastPred' s = sel $ reverse $ justs $
             map  (\instrs -> Trans.getSpecPC instrs)  s
    where
    justs [] = []
    justs (Just x:t) = Just x : justs t
    justs (Nothing:t) = justs t

    sel [] = Nothing
    sel (Just (DLX.Reg SpecPC (DLX.Val x)):t) = Just (Trans.pcTrans x)

predict2 :: Word a => Signal [DLX_Trans a] -> Signal [DLX_Trans a]
predict2 x = annotate2 x

predict3 :: Word a => Signal [DLX_Trans a] -> Signal [DLX_Trans a]
predict3 x = annotate3 x

--reorderBuffer :: Register a => (Signal [Trans a],Signal [Trans (VirtualReg a Int)]) ->
--                  (Signal [Trans a],Signal [Trans (VirtualReg a Int)],Signal Int,Signal Bool)
reorderBuffer x y = ROB.rob 100 (rmNops x,y)

--execution :: Register a => (Signal Bool,Signal [Trans (VirtualReg a Int)]) ->
--                Signal [Trans (VirtualReg a Int)]
execution x y = Hawk.lift1 shuffle (RS.rs (150,execUnits) (x,y))
-- execUnits :: Hawk.Register r => [EU DLX.DLX_Op (DLX.DLX_Cell r DLX.DLX_Word)]
execUnits :: Word a => [EU DLX_Op (DLX_Cell (Virtual DLXReg Int) a)]
execUnits = [addUnit,addUnit,subUnit,jumpUnit,jumpUnit,multUnit,divUnit,cmpUnit,moveUnit]

--shuffle (u:v:w:xs) = w : xs ++ [u,v]
--shuffle [u,v] = [v,u]
shuffle xs = xs

sumInstrSS :: Signal PC  -> Signal Int -> (Signal [DLXTrans],Signal PC )
sumInstrSS =  IFU.ifu (5,progmem sum')  --actually factorial, not sum

ilpInstrSS :: Signal PC -> Signal Int -> (Signal [DLXTrans],Signal PC)
ilpInstrSS = IFU.ifu (5,progmem ilp)
no_ilpInstrSS :: Signal PC -> Signal Int -> (Signal [DLXTrans],Signal PC)
no_ilpInstrSS = IFU.ifu (5,progmem no_ilp)
--symb_Instr :: Signal PC -> Signal Int -> (Signal [DLXTrans],Signal PC)
symb_Instr = IFU.ifu (5,progmem r)


--------------- ILD-specific components

type Byte = Int
type St = Int


-- decrSel :: Signal St -> Signal St -> Signal St
decrSel st b = mux (neq0 st) (decr st) (decr b)  -- = decr (mux (neq0 st) st b)

-- iLen :: Signal Byte -> Signal St
iLen x = x


type ProgMem a = ((a, a), [(a, a, Instr DLXReg a)])
type DataMem a = ((a, a), [(a, a, a)])
type Memory a = ((ProgMem a, DataMem a), a)

-- projections ....
progmem :: ((a,b),c) -> a
progmem ((x,y),z) = x
datamem :: ((a,b),c) -> b
datamem ((x,y),z) = y

ilp,no_ilp,sum' :: Word a => Memory a
ilp = ((((64, 1023), [(64, 64, RegReg ALU (Hawk.Add Hawk.Signed) R11 R9 R9), (65, 65, RegReg ALU (Hawk.Add Hawk.Signed) R28 R9 R9), (66, 66, RegReg ALU (Hawk.Sub Hawk.Signed) R29 R9 R9), (67, 67, RegReg ALU (Hawk.Add Hawk.Signed) R30 R9 R9), (68, 68, RegReg ALU (Hawk.Add Hawk.Signed) R24 R9 R9), (69, 69, RegReg ALU (Hawk.Sub Hawk.Signed) R25 R9 R9), (70, 70, RegReg ALU (Hawk.Add Hawk.Signed) R26 R9 R9), (71, 71, RegReg ALU (Hawk.Sub Hawk.Signed) R27 R9 R9), (72, 72, RegReg ALU (Hawk.Add Hawk.Signed) R28 R9 R9), (73, 73, RegReg ALU (Hawk.Sub Hawk.Signed) R29 R9 R9), (74, 74, RegReg ALU (Hawk.Add Hawk.Signed) R30 R9 R9), (75, 75, RegReg ALU (Hawk.Sub Hawk.Signed) R31 R9 R9), (76, 76, Jmp J ((-52))), (77, 1023, Nop)]), ((0, 16383), [(0, 63, 0), (1024, 16383, 0)])), 256)

no_ilp = ((((64, 1023), [(64, 77, RegReg ALU (Hawk.Add Hawk.Signed) R11 R11 R11), (78, 78, Jmp J ((-60))), (79, 1023, Nop)]), ((0, 16383), [(0, 63, 0), (1024, 16383, 0)])), 256)

sum' = ((((64, 1023), [(64, 64, ImmIns (ALUImm (Hawk.Add Hawk.Signed)) R1 R0 0), (65, 65, ImmIns (ALUImm (Hawk.Add Hawk.Signed)) R2 R0 5), (66, 66, ImmIns BEQZ R0 R2 12), (67, 67, RegReg ALU (Hawk.Add Hawk.Signed) R1 R1 R2), (68, 68, ImmIns (ALUImm (Hawk.Sub Hawk.Signed)) R2 R2 1), (69, 69, Jmp J ((-16))), (70, 70, ImmIns (ALUImm (Hawk.Add Hawk.Signed)) R3 R1 0), (71, 71, Jmp J ((-8))), (72, 1023, Nop)]), ((0, 16383), [(0, 63, 0), (1024, 16383, 0)])), 256)



predict x = Hawk.lift1 (map anno) x
  where anno t = let Just (Reg PC (Val pc)) = Trans.getSrcPC t 
                 in if Trans.isBranch t then Trans.addInfo (Reg SpecPC (Val pc)) t
                    else t
annotate2 x = Hawk.lift1 (map anno) x
  where anno t = let Just (Reg PC (Val pc)) = Trans.getSrcPC t 
                 in if Trans.isBranch t then Trans.addInfo (Reg SpecPC (Val $ pc+4)) t
                    else t

annotate3 x = Hawk.lift1 (map anno) x
  where anno t = if Trans.isBranch t then Trans.addInfo (Reg SpecPC (Val 264)) t
                    else t

