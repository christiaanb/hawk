all: bin/simulate

include ${HAWKROOT}/simulation/lib/unix_environment
include ${HAWKROOT}/simulation/lib/haskell_environment
include ${HAWKROOT}/models/Config.mk


HC_SRC = ${wildcard *.lhs *.hs}
HC_OBJ = ${addsuffix .o,${basename ${HC_SRC}}}

bin/simulate: ${HC_OBJ}
	hawkc -o bin/simulate ${HC_OBJ} 

test: bin/simulate
	@bin/test_columbia

clean:
	rm -f *.o *.hi bin/simulate t *.bak






































# DO NOT DELETE: Beginning of Haskell dependencies
Columbia.o Columbia.hi : Columbia.hs
Columbia.o Columbia.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Columbia.o Columbia.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Columbia.o Columbia.hi : /home/byron/hawk/simulation/lib/Trans.hi
Columbia.o Columbia.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
Columbia.o Columbia.hi : /home/byron/hawk/simulation/oa-64/OA64.hi
Columbia.o Columbia.hi : ./OA64_Utils.hi
Columbia.o Columbia.hi : ./OA64_IFU.hi
Columbia.o Columbia.hi : ./OA64_Cluster.hi
Columbia.o Columbia.hi : ./OA64_EU.hi
Columbia.o Columbia.hi : ./OA64_Queue.hi
Columbia.o Columbia.hi : ./OA64_RF.hi
Columbia.o Columbia.hi : ./OA64_Sift.hi
Main.o Main.hi : Main.hs
Main.o Main.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Main.o Main.hi : ./Columbia.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Signal.hi
OA64_Cluster.o OA64_Cluster.hi : OA64_Cluster.hs
OA64_Cluster.o OA64_Cluster.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_Cluster.o OA64_Cluster.hi : /home/byron/hawk/simulation/lib/TransSig.hi
OA64_Cluster.o OA64_Cluster.hi : /home/byron/hawk/simulation/lib/Trans.hi
OA64_Cluster.o OA64_Cluster.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
OA64_Cluster.o OA64_Cluster.hi : /home/byron/hawk/simulation/oa-64/OA64.hi
OA64_Cluster.o OA64_Cluster.hi : ./OA64_Utils.hi
OA64_Cluster.o OA64_Cluster.hi : ./OA64_Queue.hi
OA64_Cluster.o OA64_Cluster.hi : ./OA64_EU.hi
OA64_Cluster.o OA64_Cluster.hi : ./OA64_RF.hi
OA64_Cluster.o OA64_Cluster.hi : ./OA64_Sift.hi
OA64_EU.o OA64_EU.hi : OA64_EU.hs
OA64_EU.o OA64_EU.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
OA64_EU.o OA64_EU.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_EU.o OA64_EU.hi : /home/byron/hawk/simulation/lib/Trans.hi
OA64_EU.o OA64_EU.hi : /home/byron/hawk/simulation/oa-64/OA64.hi
OA64_EU.o OA64_EU.hi : ./OA64_Utils.hi
OA64_IFU.o OA64_IFU.hi : OA64_IFU.hs
OA64_IFU.o OA64_IFU.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_IFU.o OA64_IFU.hi : /home/byron/hawk/simulation/lib/Trans.hi
OA64_IFU.o OA64_IFU.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
OA64_Queue.o OA64_Queue.hi : OA64_Queue.hs
OA64_Queue.o OA64_Queue.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_RF.o OA64_RF.hi : OA64_RF.hs
OA64_RF.o OA64_RF.hi : /home/byron/hawk/simulation/lib/Trans.hi
OA64_RF.o OA64_RF.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_RF.o OA64_RF.hi : /home/byron/hawk/simulation/oa-64/OA64.hi
OA64_RF.o OA64_RF.hi : ./OA64_Utils.hi
OA64_Sift.o OA64_Sift.hi : OA64_Sift.hs
OA64_Sift.o OA64_Sift.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_Sift.o OA64_Sift.hi : /home/byron/hawk/simulation/oa-64/OA64.hi
OA64_Utils.o OA64_Utils.hi : OA64_Utils.hs
OA64_Utils.o OA64_Utils.hi : /home/byron/hawk/simulation/lib/Hawk.hi
OA64_Utils.o OA64_Utils.hi : /home/byron/hawk/simulation/lib/Trans.hi
OA64_Utils.o OA64_Utils.hi : /home/byron/hawk/simulation/lib/PreludeSig.hi
OA64_Utils.o OA64_Utils.hi : /home/byron/hawk/simulation/lib/TransSig.hi
OA64_Utils.o OA64_Utils.hi : /home/byron/hawk/simulation/oa-64/OA64.hi
# DO NOT DELETE: End of Haskell dependencies
