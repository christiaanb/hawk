<H3>Arithmetic.lhs</H3>
\begin{code}
module Arithmetic
  (
	 alu 
	,Immediate
	,Sign(..)
	,Comparison(..)
	,AluOp(..)
	,ImmediateSize(..)
  ) where

import Words
import Word
\end{code}

-- Begin Signature: Arithmetic 
<P>
The Arithmetic module defines the datatype "AluOp" to represent the
various sorts of operations you might pass to an ALU-like circuit.
The "Instruction" class defines its methods to use AluOp as the
least-common denomiator (no pun intended) of arithmetic-based instructions.



\begin{code}
type Immediate = Int
\end{code}

\begin{code}
data Sign = Signed 
          | Unsigned
      deriving (Eq,Show, Read)  
\end{code}

\begin{code}
data Comparison = LessThan 
                | LessEqual 
                | GreaterThan 
                | GreaterEqual 
                | Equal 
                | NotEqual
             deriving (Eq,Show, Read)  
\end{code}

\begin{code}
data AluOp        = Add Sign |
                  Sub Sign |
                  Mult Sign |
                  Div Sign |
                  And |
                  Not |
                  Or | Xor |
                  Sll | Srl | Sra |
                  S Comparison |
                  SetHi |               -- Set high 16 bits of value.
                  Input1 |              -- pass input1 through
                  Input2 |              -- pass input2 through
                  Invalidate            -- Invalidate the result of the
                                        --  ALU operation 
                  deriving (Eq,Show, Read)   
\end{code}


\begin{code}
data ImmediateSize = Imm16Bits | Imm26Bits
\end{code}

\begin{code}
alu :: Word w => AluOp -> w -> w -> Maybe w
\end{code}

-- End Signature: Arithmetic 
<HR>

If the ALUfunc is "Invalidate", this function returns Nothing,
otherwise it performs the assiciated ALU operation.
\begin{code}
alu Invalidate _ _
  = Nothing
alu aluFunc word1 word2
  = Just (exec_op aluFunc word1 word2)
\end{code}


signExtend is only used inside combinational circuits.
\begin{code}
signExtend :: Word w => ImmediateSize -> Immediate -> w
signExtend Imm16Bits = fromInt
signExtend Imm26Bits = fromInt
\end{code}


Integer ALU unit 


Performs addition and also returns whether overflow ocurred
\begin{code}
addOverflowCheck :: Word w =>  w -> w -> (w,Bool)
addOverflowCheck a b
  = (out,overflow)
    where
      out = a + b
      overflow = False -- out > maxBound || out < minBound
\end{code}

\begin{code}
overflowErr :: Word w => AluOp -> w -> w -> a
overflowErr op a b
  = error ("alu (" ++ show op ++ ") " ++ show a ++ " " 
          ++ show b ++ "  <-- overflow")
\end{code}


	NOTE: I'm not worrying about whether overflow
		calculations are computed correctly, except
		for signed addition and subtraction. In the
		other cases, I'm letting the bits fall where
		they may. Hopefully none of the benchmarks
		cause overflows at all.


<P>
This function performs the unsigned version of the normal signed
integer operation.
\begin{code}
unsignedWordOp :: Word w => (w->w->w) -> (w->w->w)
unsignedWordOp f a b = sign $ unsign a `f` unsign b
\end{code}


These functions convert between a Word and a vector of Bools.

\begin{code}
bitValues :: Word w => [w]
bitValues = map (2 ^) [31,30..0]
\end{code}

\begin{code}
buildVec :: Word w => w -> [Bool]
buildVec n
  = makeVec (unsign n) bitValues
    where
      makeVec :: Word w => w -> [w] -> [Bool]
      makeVec 0 [] = []
      makeVec _ [] = [] ---- should we catch this?
      makeVec n (b:bs)
	= if n >= b
	    then True : makeVec (n-b) bs
	    else False : makeVec n bs
\end{code}

\begin{code}
buildWord :: Word w => [Bool] -> w
buildWord bools
  = sign $ makeInteger bools bitValues
    where
      makeInteger [] []
	= 0
      makeInteger n []
	= error ("buildWord -- argument too large: " ++ show bools)
      makeInteger (b:bs) (n:ns)
	= if b
	    then n + makeInteger bs ns
	    else makeInteger bs ns
\end{code}

Performs an element-wise boolean operation on corresponding
pairs of bits of the argument integers.
\begin{code}
bitOp :: Word w =>  (Bool->Bool->Bool) -> (w->w->w)
bitOp f a b
  = buildWord $ zipWith f (buildVec a) (buildVec b)
\end{code}



This function assumes the ALUfunc argument is not "Invalidate".
\begin{code}
exec_op :: Word w => AluOp -> w -> w -> w

exec_op op@(Add Signed) a b
  = if overflow
      then overflowErr op a b
      else out
    where
      (out,overflow) = addOverflowCheck a b

exec_op (Add Unsigned) a b
  = unsignedWordOp (+) a b

exec_op op@(Sub Signed) a b
  = if overflow
      then overflowErr op a b
      else out
    where
      (out,overflow) = addOverflowCheck a (-b)

exec_op (Sub Unsigned) a b
  = unsignedWordOp (-) a b

exec_op (Mult Signed) a b
  = sign $ a * b

exec_op (Mult Unsigned) a b
  = unsignedWordOp (*) a b

exec_op (Div Signed) a b
  = sign $ a `div` b

exec_op (Div Unsigned) a b
  = unsignedWordOp div a b

exec_op And a b = bitOp (&&) a b

exec_op Or a b = bitOp (||) a b

-- eh, this is kinda temporary.
-- exec_op Not a b = bitOp (\x y -> not x) a b
exec_op Not a b = if a == 0 then 1 else 0

exec_op Xor a b = bitOp xor a b
		  where
		    xor False x = x
		    xor True  x = not x

exec_op Sll a b
  = buildWord $ drop shiftAmt (buildVec a) ++ replicate shiftAmt False
    where
      shiftAmt = toInt $ unsign b `mod` 32

exec_op Srl a b
  = buildWord $ replicate shiftAmt False ++ take (32 - shiftAmt) (buildVec a)
    where
      shiftAmt = toInt $ unsign b `mod` 32

exec_op Sra a b
  = buildWord $ replicate shiftAmt signBit ++ take (32 - shiftAmt) (buildVec a)
    where
      shiftAmt = toInt $ unsign b `mod` 32
      signBit = (a < 0)

exec_op (S relop) a b
  = if (a `relation` b) then 1 else 0
    where
      relation = case relop of
		   LessThan	-> (<)
		   LessEqual	-> (<=)
		   GreaterThan	-> (>)
		   GreaterEqual	-> (>=)
		   Equal	-> (==)
		   NotEqual	-> (/=)

exec_op SetHi a _
  = a * num_half	-- a * 2^n

exec_op Input1 a b
  = a

exec_op Input2 a b
  = b
\end{code}


