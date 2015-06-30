module Pipe2 where

import CLaSH.Prelude
import Hawk.Basic.BasicHawk
import Hawk.Basic.ISA

imp1Pipe :: Signal Instruction -> Signal Transaction
imp1Pipe input = writeback
  where
    rfOut     = regFile (instrToTrans <$> input) writeback
    rfOut'    = delayTrans rfOut
    aluIn     = bypass rfOut' writeback
    aluOut    = alu aluIn
    writeback = delayTrans aluOut

topEntity = imp1Pipe
