sources=ChangeLog *.lisp *.system TODO Makefile *.idl .cvsignore
all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.bak

tomcl:
	mv CVS MCVS
	tomcl $(sources)

frommac:
	mv MCVS CVS
	frommac $(sources)

shadow:
	mkdir shadow
	(cd shadow; ln -s ../MCVS CVS)
	cp $(sources) shadow
	(cd shadow; frommac $(sources))
	sleep 1
	touch shadow/before-cvs
