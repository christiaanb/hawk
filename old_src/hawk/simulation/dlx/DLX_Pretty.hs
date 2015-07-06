module DLX_Pretty where

import Trans
import Arithmetic
import DLX_Cell
import DLX_Op
import Prettier

ppDLX_Trans (Trans d op s i) =
    ppMaybe (\w -> text (show w ++ ":")) pc <:> indent 8 <:>
    ppDLX_Dest d <:> text " <- " <:>
    ppDLX_OpCells op s <:> anno
	where (pc, i') = extract_loc i
	      anno = case i' of
		       [] -> empty
		       _ -> indent 48 <:> ppList "(" ")" ppDLX_Cell i'

extract_loc [] = (Nothing, [])
extract_loc (Loc w : cs) = (Just w, cs)
extract_loc (c : cs) = (ml, c : cs')
    where (ml, cs') = extract_loc cs

ppDLX_Transs ts = text "[" <> ppsep ppDLX_Trans line ts <:> text "]"

ppDLX_Dest [d] = ppDLX_Cell d
ppDLX_Dest ds = ppDLX_Cells ds

ppDLX_OpCells (ExecOp op) xs = ppDLX_ExecOp op xs
ppDLX_OpCells (CondExecOp op1 op2) [z, x, y] =
    text "if " <:> ppDLX_Cell z <:> text " then " <:>
    ppDLX_ExecOp op2 [x, y] <:> text " else " <:> ppDLX_ExecOp op1 [x, y]
ppDLX_OpCells op s =
    ppDLX_Op op <:> text " " <:>
    ppDLX_Cells s

ppDLX_ExecOp (Add Signed) [x, y] =
    ppDLX_Cell x <:> text " + " <:> ppDLX_Cell y
ppDLX_ExecOp (Sub Signed) [x, y] =
    ppDLX_Cell x <:> text " - " <:> ppDLX_Cell y
ppDLX_ExecOp (Mult Signed) [x, y] =
    ppDLX_Cell x <:> text " * " <:> ppDLX_Cell y
ppDLX_ExecOp (Div Signed) [x, y] =
    ppDLX_Cell x <:> text " / " <:> ppDLX_Cell y
ppDLX_ExecOp (S c) [x, y] =
    ppDLX_Cell x <:+> ppComparison c <:+> ppDLX_Cell y
ppDLX_ExecOp Input1 [x, y] = ppDLX_Cell x
ppDLX_ExecOp Input2 [x, y] = ppDLX_Cell y
ppDLX_ExecOp op xs = text "DONTKNOW"

ppDLX_Cell (Reg r NotKnown) = text (show r)
ppDLX_Cell (Reg r v) = text (show r) <:> text "(" <:> ppValue v <:> text ")"
ppDLX_Cell (Loc w) = text "<" <:> text (show w) <:> text ">"
ppDLX_Cell (Imm w) = text (show w)

ppDLX_Cells cs = ppList "(" ")" ppDLX_Cell cs

ppValue NotKnown = text "NK"
ppValue Inv = text "INV"
ppValue (Val a) = text (show a)

ppDLX_Op (ExecOp o) = ppAluOp o
ppDLX_Op o = text (show o)

ppAluOp (Add Signed) = text "add"
ppAluOp (Sub Signed) = text "sub"
ppAluOp (Mult Signed) = text "mult"
ppAluOp (Div Signed) = text "div"
ppAluOp o = text (show o)

ppComparison LessThan = text "<"
ppComparison LessEqual = text "<="
ppComparison GreaterThan = text ">"
ppComparison GreaterEqual = text ">="
ppComparison Equal = text "=="
ppComparison NotEqual = text "/="
