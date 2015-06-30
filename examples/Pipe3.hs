module Pipe3 where

import CLaSH.Prelude
import Hawk.Basic.BasicHawk
import Hawk.Basic.ISA

ref2Pipe :: Signal Instruction -> Signal Transaction
ref2Pipe input = writeback
  where
    rfOut     = regFile (instrToTrans <$> input) writeback
    aluOut    = alu rfOut
    memOut    = mem aluOut
    writeback = delayTrans memOut

topEntity = ref2Pipe

-- Altera Cyclone V SoC 5CSXFC6DCF31C8ES
--
-- ALM: 1198
-- Reg: 1423
-- DSP: 3
-- MHz: 68.8
