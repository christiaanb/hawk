module Pipeline where

import Hawk
import qualified Trans as T
import TransSig
import Components
import Utils

import DLX

---------------- Pipelined DLX with forwarding and control hazards -----------

pipeline :: Word2 a b => ((((b,b),[(b,b,Instr DLXReg a)]),((b,b),[(b,b,b)])),b) -> Signal (Trans DLX_Op (DLX_Cell DLXReg b))

pipeline ((program,progmem),startPC) = writeback'
    where
      

      --- Instruction fetch ---
      pc = pipeReg (T.pcTrans startPC) pcCmd (pc' `bypassDst` branchTargetPC)
      (instr,pc') = fetchDLX program pc
      instr' = pipeReg T.nop instrFetchCmd instr

      --- Register fetch ---
      regFetch		= bypassRF  instr' writeback' 
      
      -- resolves branch data hazards
      branchTargetPC	= exec (regFetch `bypass` executed') 
      regFetch'	= pipeReg T.nop regFetchCmd regFetch 
      
      --- Execution ---
      executed = exec $ regFetch' `bypassMany` (bundleList [executed' ,writeback'])
      executed'	= delay T.nop executed

      --- Data memory ---
      writeback	= mem 4 progmem (executed' `bypass` writeback')
      writeback' = delay T.nop writeback -- $ trunc isTrapTrans writeback



      --- Hazard detection logic ---

      -- Stall PC calculation if there is a data hazard
      pcCmd		= if' dataHazard then' stall else' input


      -- Stall the IF/RF pipeline register if there is a data hazard.
      --  Generate a NoOp transaction (which cancells the branch successor)
      --  if there is a taken branch instruction at the RF stage.
      instrFetchCmd	= if' dataHazard then' stall
			   else' (if' (branchTaken branchTargetPC) then' kill
			          else' input)

      -- Generate a NoOp transaction for the RF/EX pipeline register if
      --  there is a data hazard.
      regFetchCmd	= if' dataHazard then' kill else' input

      -- Is there a data hazard requiring a partial pipeline stall?
      dataHazard = execLoadHazard *|| branchExecHazard *|| branchLoadHazard

      -- A exec-load hazard exists if a load instruction
      --  into register Rx is immediately followed by an
      --  EX-phase instruction that uses register Rx
      execLoadHazard	= isLoad regFetch' *&&
			  isExecTrans regFetch *&&
			  rawHazard (regFetch',regFetch)

      -- A branch-exec hazard exists if an EX-phase instruction
      --  is immediately followed by a branch instruction that uses
      --  the result.
      branchExecHazard	= isExecTrans regFetch' *&&
			  modifiesPC regFetch *&&
			  rawHazard (regFetch',regFetch)
			  
      -- A branch-load hazard exists if a load instruction is
      --  followed one or two instructions later by a branch
      --  instruction.
      branchLoadHazard = (isLoad regFetch' *||
			  isLoad executed'       ) *&&
			 modifiesPC regFetch		  *&&
			 (rawHazard (regFetch',regFetch) *||
			  rawHazard (executed',regFetch) )
	

