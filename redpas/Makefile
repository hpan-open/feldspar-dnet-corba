all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak *.cfsl

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp

import:
	cp ~/src/lisp/net/cddr/redpas/{package,lexer,parsys}.lisp .

export:
	-mkdir orig
	cp {package,testsuite,pattern}.lisp orig/
