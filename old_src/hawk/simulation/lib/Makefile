HC_SRC = ${wildcard *.lhs}
HC_OBJ = ${addsuffix .o,${basename ${HC_SRC}}}
HC_INC = 
# Not clear if we should really still exclude these files or not
DOCFILES = ${HC_SRC}

libhawk.a: ${HC_OBJ}
	ar -r libhawk.a ${HC_OBJ}

signature:
	@echo making Signatures.html using $(DOCFILES)
	@echo "<TITLE>Hawk Module Signatures</TITLE>"    > Signatures.html
	@echo "<BODY BGCOLOR='#ffffff' LINK='#003162' " >> Signatures.html
	@echo " VLINK='#CC3333' TEXT='#000000'>"        >> Signatures.html
	@echo "<H4>Hawk Module Signatures</H4>"         >> Signatures.html
	@echo "<font size=2>"                           >> Signatures.html
	@./mkSignature $(DOCFILES)                        >> Signatures.html
	@echo "</font><HR><BR><PRE>`date`</PRE>"        >> Signatures.html
	cp Signatures.html ../doc/Signatures.html
	chmod a+r ../doc/Signatures.html

clean:
	rm -f *.o *.hi libhawk.a *.bak

include ./unix_environment
include ./haskell_environment
include ./Config.mk





# DO NOT DELETE: Beginning of Haskell dependencies
Arithmetic.o : Arithmetic.lhs
Arithmetic.o : ./Words.hi
Cell.o : Cell.lhs
Cell.o : ./Register.hi
Cell.o : ./Words.hi
Devices.o : Devices.lhs
Devices.o : ./TransSig.hi
Devices.o : ./Trans.hi
Devices.o : ./PreludeSig.hi
Devices.o : ./Signal.hi
Devices.o : ./Words.hi
Devices.o : ./Register.hi
Devices.o : ./Arithmetic.hi
Devices.o : ./Cell.hi
Devices.o : ./Memory.hi
Devices.o : ./Utilities.hi
Devices.o : ./Instruction.hi
Devices.o : ./StateArray.hi
Hawk.o : Hawk.lhs
Hawk.o : ./Arithmetic.hi
Hawk.o : ./Cell.hi
Hawk.o : ./Devices.hi
Hawk.o : ./HawkIO.hi
Hawk.o : ./Memory.hi
Hawk.o : ./Register.hi
Hawk.o : ./STEx.hi
Hawk.o : ./Signal.hi
Hawk.o : ./Utilities.hi
Hawk.o : ./VRegister.hi
Hawk.o : ./Words.hi
Hawk.o : ./Instruction.hi
Hawk.o : ./Probe.hi
Hawk.o : ./PipeReg.hi
Hawk.o : ./Init.hi
HawkIO.o : HawkIO.lhs
HawkIO.o : ./Memory.hi
Init.o : Init.lhs
Init.o : ./Signal.hi
Instruction.o : Instruction.lhs
Instruction.o : ./Arithmetic.hi
Instruction.o : ./Memory.hi
Memory.o : Memory.lhs
Memory.o : ./Arithmetic.hi
Memory.o : ./Words.hi
PipeReg.o : PipeReg.lhs
PipeReg.o : ./Trans.hi
PipeReg.o : ./Signal.hi
PipeReg.o : ./Register.hi
PipeReg.o : ./Instruction.hi
PreludeSig.o : PreludeSig.lhs
PreludeSig.o : ./Signal.hi
Prettier.o : Prettier.lhs
Probe.o : Probe.lhs
Probe.o : ./Arithmetic.hi
Probe.o : ./Cell.hi
Probe.o : ./Devices.hi
Probe.o : ./Memory.hi
Probe.o : ./Trans.hi
Probe.o : ./VRegister.hi
Probe.o : ./Signal.hi
Probe.o : ./PipeReg.hi
Probe.o : ./Utilities.hi
Probe.o : ./Trans.hi
Probe.o : ./Prettier.hi
Register.o : Register.lhs
STEx.o : STEx.lhs
Signal.o : Signal.lhs
StateArray.o : StateArray.lhs
StateArray.o : ./Signal.hi
StateArray.o : ./Memory.hi
StateArray.o : ./Words.hi
StateArray.o : ./Memory.hi
StateArray.o : ./Signal.hi
StateArray.o : ./Words.hi
StateArray.o : ./Memory.hi
Symbo.o : Symbo.lhs
Symbo.o : ./Words.hi
Trans.o : Trans.lhs
Trans.o : ./Words.hi
Trans.o : ./Arithmetic.hi
Trans.o : ./Cell.hi
Trans.o : ./Memory.hi
Trans.o : ./Register.hi
Trans.o : ./Instruction.hi
TransSig.o : TransSig.lhs
TransSig.o : ./Instruction.hi
TransSig.o : ./Signal.hi
TransSig.o : ./Register.hi
TransSig.o : ./Words.hi
TransSig.o : ./Arithmetic.hi
TransSig.o : ./Trans.hi
TransSig.o : ./Trans.hi
TransSig.o : ./Cell.hi
Utilities.o : Utilities.lhs
VRegister.o : VRegister.lhs
VRegister.o : ./Register.hi
VRegister.o : ./Cell.hi
VRegister.o : ./Trans.hi
Words.o : Words.lhs
# DO NOT DELETE: End of Haskell dependencies
