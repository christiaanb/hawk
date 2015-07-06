%.o : %.hs
	hawkc -c $< -o $@
#	${HC} ${HC_OPTS} -c $< -o $@
#	${HC} ${HC_OPTS} -c $< -o $@ 2>&1 | grep -v "module version"

%.o : %.lhs
	hawkc -c $< -o $@
#	${HC} ${HC_OPTS} -c $< -o $@
#	${HC} ${HC_OPTS} -c $< -o $@ 2>&1 | grep -v "module version"

%.hi : %.o
	@if [ ! -f $@ ] ; then \
	    echo Panic! $< exists, but $@ does not. \
	    exit 1; \
	else exit 0 ; \
	fi							

depend:
	mkdependHawk ${HC_SRC}
