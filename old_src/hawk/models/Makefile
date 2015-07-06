all: depend
	(cd p6 ; make)
	(cd pipeline+speculative; make )
	(cd simple; make)
	(cd columbia; make)

depend:
	(cd p6 ; make depend)
	(cd columbia;make depend)
	(cd pipeline+speculative ; make depend)
	(cd simple ; make depend)

clean:
	(cd p6 ; make clean)
	(cd pipeline+speculative; make clean )
	(cd simple; make clean)
	(cd columbia;make clean)

test:
	@make realtest | grep -v directory



realtest:
	@(cd p6; make test)
	@(cd columbia; make test)
	




