HC_SRC = ${wildcard *.lhs}
HC_INC = ../lib
DLX_OBJ = DLX.o DLX_Cell.o DLX_Reg.o DLX_Op.o

all: libdlx.a

libdlx.a: ${DLX_OBJ}
	ar -r libdlx.a ${DLX_OBJ}

clean:
	rm -f *.o *.hi libdlx.a *.bak

include ../lib/unix_environment
include ../lib/haskell_environment
include ../lib/Config.mk































# DO NOT DELETE: Beginning of Haskell dependencies
DLX.o : DLX.lhs
DLX.o : ../lib/Hawk.hi
DLX.o : ../lib/Trans.hi
DLX.o : ../lib/Prettier.hi
DLX.o : ./DLX_Cell.hi
DLX.o : ./DLX_Reg.hi
DLX.o : ./DLX_Op.hi
DLX.o : ./DLX_Pretty.hi
DLX_Cell.o : DLX_Cell.lhs
DLX_Cell.o : ../lib/Cell.hi
DLX_Cell.o : ../lib/Register.hi
DLX_Cell.o : ../lib/Words.hi
DLX_Op.o : DLX_Op.lhs
DLX_Op.o : ../lib/Arithmetic.hi
DLX_Op.o : ../lib/Memory.hi
DLX_Op.o : ../lib/Instruction.hi
DLX_Op.o : ../lib/Probe.hi
DLX_Reg.o : DLX_Reg.lhs
# DO NOT DELETE: End of Haskell dependencies
