all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.nfasl *.bak
	$(MAKE) -C idlcomp clean
	$(MAKE) -C luna clean

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp
	cd idlcomp; make setmcl
	/Developer/Tools/SetFile -t TEXT -c CCL2 examples/*/*.lisp

prep-cvs:
	cd idlcomp; make prep-cvs
