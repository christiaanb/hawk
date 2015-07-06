HC_SRC = ${wildcard *.hs}
HC_INC = ../lib
OA64_OBJ = OA64.o OA64_Cell.o OA64_Reg.o OA64_Op.o OA64_RegInst.o OA64_Assm.o

all: liboa64.a

liboa64.a: ${OA64_OBJ}
	ar -r liboa64.a ${OA64_OBJ}

clean:
	rm -f *.o *.hi liboa64.a *.bak

include ../lib/unix_environment
include ../lib/haskell_environment
include ../lib/Config.mk

OA64_Reg.o: OA64_Reg.hs
	hawkc -H75m -c OA64_Reg.hs


































# DO NOT DELETE: Beginning of Haskell dependencies
OA64.o : OA64.hs
OA64.o : ../lib/Hawk.hi
OA64.o : ../lib/Trans.hi
OA64.o : ./OA64_Cell.hi
OA64.o : ./OA64_Reg.hi
OA64.o : ./OA64_Op.hi
OA64.o : ./OA64_Assm.hi
OA64.o : ../lib/Probe.hi
OA64_Assm.o : OA64_Assm.hs
OA64_Assm.o : ../lib/Hawk.hi
OA64_Assm.o : ../lib/Trans.hi
OA64_Assm.o : ./OA64_Cell.hi
OA64_Assm.o : ./OA64_Reg.hi
OA64_Assm.o : ./OA64_RegInst.hi
OA64_Assm.o : ./OA64_Op.hi
OA64_Cell.o : OA64_Cell.hs
OA64_Cell.o : ../lib/Cell.hi
OA64_Cell.o : ../lib/Register.hi
OA64_Cell.o : ../lib/Words.hi
OA64_Op.o : OA64_Op.hs
OA64_Op.o : ../lib/Hawk.hi
OA64_Op.o : ../lib/Trans.hi
OA64_Op.o : ./OA64_Cell.hi
OA64_Op.o : ./OA64_Reg.hi
OA64_Reg.o : OA64_Reg.hs
OA64_RegInst.o : OA64_RegInst.hs
OA64_RegInst.o : ../lib/Register.hi
OA64_RegInst.o : ./OA64_Reg.hi
# DO NOT DELETE: End of Haskell dependencies
