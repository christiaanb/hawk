all: simulate

include ${HAWKROOT}/simulation/lib/unix_environment
include ${HAWKROOT}/simulation/lib/haskell_environment
include ${HAWKROOT}/models/Config.mk


HC_SRC = ${wildcard *.lhs *.hs}
HC_OBJ = ${addsuffix .o,${basename ${filter-out t.hs IQ.lhs,${HC_SRC}}}}

simulate: ${HC_OBJ} Main.o
	hawkc -o simulate ${HC_OBJ} 

t: ${HC_OBJ} t.o
	hawkc -o t ${HC_OBJ}

test: simulate
	@test_p6

clean:
	rm -f *.o *.hi simulate t *.bak






# DO NOT DELETE: Beginning of Haskell dependencies
AQ.o AQ.hi : AQ.lhs
AQ.o AQ.hi : ./Utils.hi
AQ.o AQ.hi : /home/byron/hawk/simulation/lib/Hawk.hi
BoundedSet.o BoundedSet.hi : BoundedSet.lhs
EUs.o EUs.hi : EUs.lhs
EUs.o EUs.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
EUs.o EUs.hi : /home/byron/hawk/simulation/lib/Hawk.hi
EUs.o EUs.hi : /home/byron/hawk/simulation/lib/Trans.hi
EUs.o EUs.hi : /home/byron/hawk/simulation/dlx/DLX.hi
IFU.o IFU.hi : IFU.lhs
IFU.o IFU.hi : /home/byron/hawk/simulation/lib/Trans.hi
IFU.o IFU.hi : /home/byron/hawk/simulation/lib/Hawk.hi
IFU.o IFU.hi : /home/byron/hawk/simulation/dlx/DLX.hi
IFU.o IFU.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
IQ.o IQ.hi : IQ.lhs
Main.o Main.hi : Main.lhs
Main.o Main.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Cell.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Reg.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Op.hi
Main.o Main.hi : ./Processor.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Signal.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Trans.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Probe.hi
Predict.o Predict.hi : Predict.lhs
Predict.o Predict.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Predict.o Predict.hi : /home/byron/hawk/simulation/lib/Trans.hi
Predict.o Predict.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Predict.o Predict.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Processor.o Processor.hi : Processor.lhs
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/Trans.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
Processor.o Processor.hi : ./Utils.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Processor.o Processor.hi : ./Predict.hi
Processor.o Processor.hi : ./IFU.hi
Processor.o Processor.hi : ./RS.hi
Processor.o Processor.hi : ./ROB.hi
Processor.o Processor.hi : ./EUs.hi
RAT.o RAT.hi : RAT.lhs
RAT.o RAT.hi : /home/byron/hawk/simulation/lib/Hawk.hi
RAT.o RAT.hi : /home/byron/hawk/simulation/dlx/DLX.hi
RAT.o RAT.hi : /home/byron/hawk/simulation/lib/Trans.hi
RAT.o RAT.hi : ./Utils.hi
RF.o RF.hi : RF.lhs
RF.o RF.hi : ./Utils.hi
RF.o RF.hi : /home/byron/hawk/simulation/lib/Hawk.hi
ROB.o ROB.hi : ROB.lhs
ROB.o ROB.hi : /home/byron/hawk/simulation/lib/Hawk.hi
ROB.o ROB.hi : /home/byron/hawk/simulation/lib/Trans.hi
ROB.o ROB.hi : ./RF.hi
ROB.o ROB.hi : ./AQ.hi
ROB.o ROB.hi : ./RAT.hi
ROB.o ROB.hi : ./RF.hi
ROB.o ROB.hi : ./AQ.hi
ROB.o ROB.hi : ./RAT.hi
ROB.o ROB.hi : ./ROB_insert.hi
ROB.o ROB.hi : ./ROB_retire.hi
ROB.o ROB.hi : /home/byron/hawk/simulation/dlx/DLX.hi
ROB_insert.o ROB_insert.hi : ROB_insert.lhs
ROB_insert.o ROB_insert.hi : /home/byron/hawk/simulation/lib/Hawk.hi
ROB_insert.o ROB_insert.hi : /home/byron/hawk/simulation/lib/Trans.hi
ROB_insert.o ROB_insert.hi : ./RF.hi
ROB_insert.o ROB_insert.hi : ./AQ.hi
ROB_insert.o ROB_insert.hi : ./RAT.hi
ROB_insert.o ROB_insert.hi : ./AQ.hi
ROB_insert.o ROB_insert.hi : ./RAT.hi
ROB_insert.o ROB_insert.hi : ./RF.hi
ROB_insert.o ROB_insert.hi : ./Utils.hi
ROB_insert.o ROB_insert.hi : /home/byron/hawk/simulation/dlx/DLX.hi
ROB_retire.o ROB_retire.hi : ROB_retire.lhs
ROB_retire.o ROB_retire.hi : /home/byron/hawk/simulation/lib/Hawk.hi
ROB_retire.o ROB_retire.hi : /home/byron/hawk/simulation/lib/Trans.hi
ROB_retire.o ROB_retire.hi : ./RF.hi
ROB_retire.o ROB_retire.hi : ./AQ.hi
ROB_retire.o ROB_retire.hi : ./RAT.hi
ROB_retire.o ROB_retire.hi : ./RF.hi
ROB_retire.o ROB_retire.hi : ./AQ.hi
ROB_retire.o ROB_retire.hi : ./RAT.hi
ROB_retire.o ROB_retire.hi : /home/byron/hawk/simulation/dlx/DLX.hi
ROB_retire.o ROB_retire.hi : ./Utils.hi
RS.o RS.hi : RS.lhs
RS.o RS.hi : /home/byron/hawk/simulation/lib/Hawk.hi
RS.o RS.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
RS.o RS.hi : /home/byron/hawk/simulation/lib/Trans.hi
RS.o RS.hi : /home/byron/hawk/simulation/lib/TransSig.hi
RS.o RS.hi : ./BoundedSet.hi
RS.o RS.hi : ./EUs.hi
RS.o RS.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Utils.o Utils.hi : Utils.lhs
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/Trans.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/dlx/DLX.hi
t.o t.hi : t.hs
t.o t.hi : /home/byron/hawk/simulation/lib/Hawk.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Trans.hi
t.o t.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
t.o t.hi : /home/byron/hawk/simulation/dlx/DLX.hi
t.o t.hi : ./Predict.hi
t.o t.hi : ./Processor.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Arithmetic.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Words.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Probe.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Symbo.hi
t.o t.hi : ./IFU.hi
t.o t.hi : ./RS.hi
t.o t.hi : ./ROB.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Prettier.hi
t.o t.hi : /home/byron/hawk/simulation/dlx/DLX_Pretty.hi
# DO NOT DELETE: End of Haskell dependencies
