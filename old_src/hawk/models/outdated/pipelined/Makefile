HC = hawkc
 
.SUFFIXES: .hs .o .lhs .hi
 
.hs.hi:
	$(HC) -c $<
.lhs.hi:
	$(HC) -c $<

simulate: Main.hi
	$(HC) -o simulate *.o

mf:
	mkdependHawk *.hs *.lhs

clean:
	rm -f *.o *.hi simulate *.bak



# DO NOT DELETE: Beginning of Haskell dependencies
Components.o Components.hi : Components.hs
Components.o Components.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Components.o Components.hi : /home/byron/hawk/simulation/lib/Trans.hi
Components.o Components.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Components.o Components.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Pipeline.o Pipeline.hi : Pipeline.hs
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/lib/Trans.hi
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Pipeline.o Pipeline.hi : ./Components.hi
Pipeline.o Pipeline.hi : ./Utils.hi
Pipeline.o Pipeline.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Utils.o Utils.hi : Utils.hs
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/Trans.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/lib/TransSig.hi
Utils.o Utils.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Main.o Main.hi : Main.lhs
Main.o Main.hi : /home/byron/hawk/simulation/lib/Hawk.hi
Main.o Main.hi : ./Pipeline.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Signal.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Cell.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Reg.hi
Main.o Main.hi : /home/byron/hawk/simulation/dlx/DLX_Op.hi
Main.o Main.hi : /home/byron/hawk/simulation/lib/Trans.hi
# DO NOT DELETE: End of Haskell dependencies
