msources = ChangeLog *.lisp *.system TODO  *.idl 
usources = Makefile .cvsignore
sources  = $(msources) $(usources)


all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak

symlinks: idl-compiler-support.lisp idl-compiler.lisp scanner-support.lisp idl-scanner-parser.lisp

idl-compiler.lisp:
	ln -s idlcomp/idl-compiler.lisp idl-compiler.lisp
idl-compiler-support.lisp:
	ln -s idlcomp/idl-compiler-support.lisp idl-compiler-support.lisp
scanner-support.lisp:
	ln -s idlcomp/lisp-scanner/scanner-support.lisp scanner-support.lisp
idl-scanner-parser.lisp:
	ln -s idlcomp/idl-scanner-parser.lisp idl-scanner-parser.lisp


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
