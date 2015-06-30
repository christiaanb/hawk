module Pipe1 where

import CLaSH.Prelude
import Hawk.Basic.BasicHawk
import Hawk.Basic.ISA

reflPipe :: Signal Instruction -> Signal Transaction
reflPipe input = writeBack
  where
    rfOut     = regFile (instrToTrans <$> input) writeBack
    aluOut    = alu rfOut
    writeBack = delayTrans aluOut

topEntity = reflPipe
