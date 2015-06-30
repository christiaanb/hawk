module Pipe4 where

import CLaSH.Prelude
import Hawk.Basic.BasicHawk
import Hawk.Basic.ISA

imp2Pipe :: Signal Instruction -> Signal Transaction
imp2Pipe input = writeback
  where
    rfOut     = regFile (instrToTrans <$> input) writeback
    rfOut'    = delayTrans rfOut
    aluIn     = bypass (bypass rfOut' writeback)
                       aluOut'
    aluOut    = alu aluIn
    aluOut'   = delayTrans aluOut
    memIn     = bypass aluOut' writeback
    memOut    = mem memIn
    writeback = delayTrans memOut

topEntity = imp2Pipe

-- Altera Cyclone V SoC 5CSXFC6DCF31C8ES
--
-- ALM: 1345
-- Reg: 1738
-- DSP: 3
-- MHz: 80.3
