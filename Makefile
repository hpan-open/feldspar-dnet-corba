all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp
	cd idlcomp; make setmcl

prep-cvs:
	cd idlcomp; make prep-cvs
