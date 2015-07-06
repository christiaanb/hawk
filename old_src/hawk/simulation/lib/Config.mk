HC_INCI = ${addprefix -i,${HC_INC}}
HC_OPTS = ${HAWK_OPTS} ${$*_HC_OPTS} ${HC_INCI}
DEP_OPTS = -fglasgow-exts ${HC_INCI}
HC_LDOPTS = ${addprefix -L,${HC_INC}}

HAWK_OPTS = -cpp -optP-imacros -optP${HAWKLIB}/hawk-macros.h

%.o : %.lhs
	${HC} ${HC_OPTS} -c $< -o $@

%.o : %.hs
	${HC} ${HC_OPTS} -c $< -o $@
#	${HC} ${HC_OPTS} -c $< -o $@ 2>&1 | grep -v "module version"

%.hi : %.o
	@if [ ! -f $@ ] ; then \
	    echo Panic! $< exists, but $@ does not. \
	    exit 1; \
	else exit 0 ; \
	fi							

depend:
	mkdependHS ${DEP_OPTS} ${HC_SRC}
