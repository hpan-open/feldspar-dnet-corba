msources = ChangeLog *.lisp *.system TODO  *.idl 
usources = Makefile .cvsignore
sources  = $(msources) $(usources)


all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak

tomcl:
	mv CVS MCVS
	tomcl $(msources)

frommac:
	mv MCVS CVS
	frommac $(msources)

shadow:
	mkdir shadow
	(cd shadow; ln -s ../MCVS CVS)
	cp $(sources) shadow
	(cd shadow; frommac $(msources))
	sleep 1
	touch shadow/before-cvs
