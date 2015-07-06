all: simulate

include ${HAWKROOT}/simulation/lib/unix_environment
include ${HAWKROOT}/simulation/lib/haskell_environment
include ${HAWKROOT}/models/Config.mk


HC_SRC = Components.lhs PipeUtils.lhs Main.lhs PipePredict.lhs Pipeline.lhs t.hs
HC_OBJ = ${addsuffix .o,${basename ${filter-out t.hs,${HC_SRC}}}}

simulate: ${HC_OBJ}
	hawkc -o simulate ${HC_OBJ} 

t: ${HC_OBJ}
	hawkc -o t ${HC_OBJ}

clean:
	rm -f *.o *.hi simulate t *.bak



































# DO NOT DELETE: Beginning of Haskell dependencies
Components.o Components.hi : Components.lhs
Components.o Components.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Components.o Components.hi : /home/byron/hawk/simulation/lib/Trans.hi
Components.o Components.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Components.o Components.hi : /home/byron/hawk/simulation/dlx/DLX.hi
PipeUtils.o PipeUtils.hi : PipeUtils.lhs
PipeUtils.o PipeUtils.hi : /home/byron/hawk/simulation/lib/Hawk.hi
PipeUtils.o PipeUtils.hi : /home/byron/hawk/simulation/lib/Trans.hi
PipeUtils.o PipeUtils.hi : /home/byron/hawk/simulation/lib/TransSig.hi
PipeUtils.o PipeUtils.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Main.o Main.hi : Main.lhs
Main.o Main.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Main.o Main.hi : ./Pipeline.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Signal.hi
Main.o Main.hi : ./PipeUtils.hi
PipePredict.o PipePredict.hi : PipePredict.lhs
PipePredict.o PipePredict.hi : /home/byron/hawk/simulation/lib/Hawk.hi
PipePredict.o PipePredict.hi : /home/byron/hawk/simulation/lib/Trans.hi
PipePredict.o PipePredict.hi : /home/byron/hawk/simulation/lib/TransSig.hi
PipePredict.o PipePredict.hi : /home/byron/hawk/simulation/dlx/DLX.hi
PipePredict.o PipePredict.hi : ./PipeUtils.hi
Pipeline.o Pipeline.hi : Pipeline.lhs
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Pipeline.o Pipeline.hi : ./PipePredict.hi
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/lib/Trans.hi
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Pipeline.o Pipeline.hi : ./Components.hi
Pipeline.o Pipeline.hi : ./PipeUtils.hi
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/dlx/DLX.hi
t.o t.hi : t.hs
t.o t.hi : /home/byron/hawk/simulation/lib/Signal.hi
t.o t.hi : /home/byron/hawk/simulation/dlx/DLX.hi
t.o t.hi : ./Pipeline.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Arithmetic.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Words.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Probe.hi
t.o t.hi : /home/byron/hawk/simulation/lib/Symbo.hi
# DO NOT DELETE: End of Haskell dependencies
