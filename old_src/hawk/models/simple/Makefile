all: simulate

include ${HAWKROOT}/simulation/lib/unix_environment
include ${HAWKROOT}/simulation/lib/haskell_environment
include ${HAWKROOT}/models/Config.mk


HC_SRC = Components.lhs Main.lhs Processor.lhs Utils.lhs 
HC_OBJ = ${addsuffix .o,${basename ${filter-out t.lhs,${HC_SRC}}}}

simulate: ${HC_OBJ}
	hawkc -o simulate ${HC_OBJ} 

clean:
	rm -f *.o *.hi simulate *.bak


































# DO NOT DELETE: Beginning of Haskell dependencies
Components.o Components.hi : Components.lhs
Components.o Components.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Components.o Components.hi : /home/byron/hawk/simulation/lib/Trans.hi
Components.o Components.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Components.o Components.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Main.o Main.hi : Main.lhs
Main.o Main.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Main.o Main.hi : ./Processor.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Signal.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Cell.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Reg.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Op.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Trans.hi
Processor.o Processor.hi : Processor.lhs
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/lib/Trans.hi
Processor.o Processor.hi : ./Components.hi
Processor.o Processor.hi : ./Utils.hi
Processor.o Processor.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Utils.o Utils.hi : Utils.lhs
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/Trans.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/TransSig.hi
# DO NOT DELETE: End of Haskell dependencies
