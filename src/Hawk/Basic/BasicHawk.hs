{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Hawk.Basic.BasicHawk
  ( -- * Types
    Signal
  , Transaction (..)
  , Instruction (..)
  , Cell
  , DestCell
  , Src1Cell
  , Src2Cell
  , Elems
  , Env
  , RegFile
  , DataMem
  -- * Basic signal operations
  , fmap
  , liftA2
  , liftA3
  , signal
  , (.==.)
  , bundle
  , unbundle
  , register
  -- * Environment operations
  , repeat
  , readEnv
  , extEnv
  -- * Transactions and Cell operations
  , getCellName
  , getCellValue
  , setCellValue
  , bypassCell
  , isMemTrans
  , isLoadTrans
  , isALUTrans
  , hazardTrans
  , bypassTrans
  , aluTrans
  , memTrans
  , nopTrans
  , instrToTrans
  -- * Pipeline Components
  , hiAddr
  , delayTrans
  , regFile
  , alu
  , mem
  , bypass
  , removeHazards
  , removeMemInstrs
  )
where

import Control.Applicative (liftA2,liftA3)
import CLaSH.Promoted.Nat  (SNat)
import CLaSH.Signal        (Signal,(.==.),bundle,register,signal,unbundle)
import CLaSH.Sized.Signed  (Signed)
import CLaSH.Sized.Vector  (Vec,(!!),replace,repeat)
import Data.Functor        (fmap)
import GHC.TypeLits        (KnownNat,Nat,type (^))
import Prelude             hiding ((!!),repeat)

import Data.Proxy

import Hawk.Basic.ISA

-- | Transactions package up  all the state associated as it flows trough the
-- pipeline. At early stages of the pipeline, not much is known about the
-- instruction's operands. As the instruction proceeds trough the pipeline,
-- operand information is incrementally filled in.
data Transaction = Trans DestCell Opcode Src1Cell Src2Cell
  deriving (Show,Eq)

data Instruction = Instr RegName Opcode RegName RegName

-- | A Cell contains known information regarding a register's value, relative
-- to the transaction it is a part of.
data Cell = Cell RegName (Maybe RegValue)
  deriving (Show,Eq)

type DestCell = Cell
type Src1Cell = Cell
type Src2Cell = Cell

type family Elems a :: Nat
type instance Elems RegName    = 8
type instance Elems (Signed n) = 2^n

-- | This type implements register files and memories using fixed size vectors
type Env a b = Vec (Elems a) b

-- | The type of register files. Register files are finite mappings from
-- register names to register values.
type RegFile = Env RegName RegValue

-- | The type of data memories. Data memories are finite mappings from register
-- values (considered as addresses) to register values.
type DataMem = Env RegValue RegValue

-- | Reads the current binding from the environment
readEnv :: (Enum a, KnownNat (Elems a)) => Env a b -> a -> b
readEnv e i = e !! (fromEnum i)

-- | Extends the given environment by overwriting the associated binding.
extEnv :: (Enum a, KnownNat (Elems a)) => Env a b -> a -> b -> Env a b
extEnv v i a = replace i a v

-- | @getCellName@ retrieves the @registerName@ field of the cell
getCellName :: Cell -> RegName
getCellName (Cell regName _) = regName

-- | @getCellValue@ retrieves the value associated with a cell. It is a runtime
-- error if the cell has no value.
getCellValue :: Cell -> RegValue
getCellValue (Cell _ (Just val)) = val

-- | Overwrite the existing value in a cell
setCellValue :: Cell -> RegValue -> Cell
setCellValue (Cell regName _) regVal
  = Cell regName (Just regVal)

-- | Overwrite the contents of the first cell with the contents of the second
-- cell, but only if both cells contains the same register name.
bypassCell :: Cell -> Cell -> Cell
bypassCell cell1 cell2
  = if getCellName cell1 == getCellName cell2
      then cell2
      else cell1

-- | Is the transaction a memory instruction?
isMemTrans :: Transaction -> Bool
isMemTrans (Trans _ opcode _ _)
  = opcode == Load || opcode == Store

-- | Is the transaction a load instruction?
isLoadTrans :: Transaction -> Bool
isLoadTrans (Trans _ Load _ _) = True
isLoadTrans _                 = False

-- | Is the transaction an alu instruction?
isALUTrans :: Transaction -> Bool
isALUTrans (Trans _ opcode _ _)
  = case opcode of
      (LdImm _) -> True
      Add       -> True
      Sub       -> True
      Mul       -> True
      otherwise -> False

-- | Is the second transaction argument dependent on the first transaction
-- argument
hazardTrans :: Transaction -> Transaction -> Bool
hazardTrans (Trans dst _ _ _) (Trans _ _ src1 src2)
  = dstName /= R0 &&
    (dstName == getCellName src1 || dstName == getCellName src2)
  where
    dstName = getCellName dst

-- | Overwite the first transaction's source cells with the second transaction's
-- destination cell. Only overwrite those source cells that have the same
-- register name as the destination cell.
bypassTrans :: Transaction -> Transaction -> Transaction
bypassTrans (Trans dstA opcA src1A src2A) (Trans dstB _ _ _)
  = Trans dstA opcA (bypassCell src1A dstB) (bypassCell src2A dstB)

-- | If the transaction's opcode is an ALU instruction, then perform the ALU
-- operation on the source cell values and overwrite the destination cell value
-- with the result.
aluTrans :: Transaction -> Transaction
aluTrans (Trans dst opcode src1 src2)
  = Trans dst' opcode src1 src2
  where
    dst' = Cell dstName
                (case (dstName,opcode) of
                    (R0,_)        -> Just 0
                    (_,LdImm imm) -> Just imm
                    (_,Add)       -> Just (val1 + val2)
                    (_,Sub)       -> Just (val1 - val2)
                    (_,Mul)       -> Just (val1 * val2)
                    otherwise     -> dstVal)
    (Cell dstName dstVal) = dst
    val1 = getCellValue src1
    val2 = getCellValue src2

-- | If the transactions' opcode is a memory operation, then update the given
-- transaction's destination cell and environment according to the opcode.
-- Otherwise return the transaction and environment unchanged.
memTrans :: DataMem -> Transaction -> (DataMem,Transaction)
memTrans mem (Trans dst opcode src1 src2)
  = (mem', Trans dst' opcode src1 src2)
  where
    mem' = case opcode of
             Store     -> extEnv mem memAddr storeVal
             otherwise -> mem
    dst' = case opcode of
             Load -> if dstName == R0
                        then dst
                        else setCellValue dst loadVal
             otherwise -> dst
    (Cell dstName _) = dst
    loadVal = readEnv mem memAddr
    memAddr = abs (getCellValue src1) `mod` hiAddr
    storeVal = getCellValue src2

-- | An \"inert\" transaction, typically used to initialize delay registers and
-- as \"bubble\" values in pipelines.
nopTrans = Trans nopCell Nop nopCell nopCell
  where
    nopCell = (Cell R0 (Just 0))

-- | This parameter specifies how large (in words) the data memory will be.
-- Addresses in the data memory range from zero to \;'hiAddr\' - 1.
hiAddr = 256

-- | Delays a transaction signal by one clock period. The output at the zeroth
-- clock period is a nopTrans
delayTrans :: Signal Transaction -> Signal Transaction
delayTrans = register nopTrans

-- | Holds the state of the architectural register file. Incoming transactions
-- have their source operand cell's values set according to the current state of
-- the register file. The second argument's destination cell is used to update
-- the state of the register file. During each clock cycle, the register file is
-- first updated, and then the updated results are used to assign the first
-- argument's source cells. The register file's contents are initially all set
-- to zero.
regFile :: Signal Transaction -> Signal Transaction -> Signal Transaction
regFile input writeBack = liftA2 setCells input registers
  where
    setCells (Trans dst opcode src1 src2) regs
      = Trans dst' opcode src1' src2'
      where
        dst'    = mkConstantR0Cell dst opcode
        src1'   = setCellValue src1 src1Val
        src2'   = setCellValue src2 src2Val
        src1Val = readEnv regs (getCellName src1)
        src2Val = readEnv regs (getCellName src2)

        -- Make a constant destination cell if either the destination cell is R0
        -- or if the opcode doesn't assign to a register.
        mkConstantR0Cell (Cell R0 _) _     = Cell R0 (Just 0)
        mkConstantR0Cell _           Nop   = Cell R0 (Just 0)
        mkConstantR0Cell _           Store = Cell R0 (Just 0)
        mkConstantR0Cell cell        _     = cell

    registers = liftA2 updateRegs (register initRegs registers) writeBack

    updateRegs regs (Trans dst _ _ _)
      = extEnv regs (getCellName dst) (getCellValue dst)

    initRegs = repeat 0

-- | ALU pipeline Components
alu :: Signal Transaction -> Signal Transaction
alu = fmap aluTrans

-- | Memory pipeline component. Memory addresses range from 0 to \'hiAddr\'
-- (word addressable only). Memory content are all initialized to zero.
mem :: Signal Transaction -> Signal Transaction
mem input
  = out
  where
    (mem,out) = unbundle (liftA2 memTrans (register initMem mem) input)
    initMem   = repeat 0

-- | Bypass pipeline component
bypass :: Signal Transaction -> Signal Transaction -> Signal Transaction
bypass = liftA2 bypassTrans

-- | This component enforces the restriction that no memory instruction
-- immediately follows an ALU instruction it is dependent upon. It enforces this
-- by replacing dependent memory instructions by nopTrans. While this circuit
-- would not be used in a real micro-architecture, it is useful for verifying
-- certain correctness properties.
removeHazards :: Signal Transaction -> Signal Transaction
removeHazards ts
  = liftA2 remHazTrans ts prevTs
  where
    prevTs = delayTrans ts
    remHazTrans trans prevTrans
      = if isLoadTrans prevTrans &&
           isALUTrans trans &&
           hazardTrans prevTrans trans
           then nopTrans
           else trans

-- | Converts all memory instructions to nopTrans's
removeMemInstrs :: Signal Transaction -> Signal Transaction
removeMemInstrs
  = fmap remTrans
  where
    remTrans trans = if isMemTrans trans
                        then nopTrans
                        else trans

instrToTrans :: Instruction -> Transaction
instrToTrans (Instr dst op src1 src2) = Trans (Cell dst Nothing) op (Cell src1 Nothing) (Cell src2 Nothing)
