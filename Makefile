all:
FASL="*.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.nfasl *.cfsl"

clean:
	eval rm -f $(FASL) *.bak
	$(MAKE) -C idlcomp clean
	$(MAKE) -C luna clean

clean-fasl:
	-cd fasl && eval rm -f $(FASL) && rm -rf luna idlcomp

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp
	cd idlcomp; make setmcl
	/Developer/Tools/SetFile -t TEXT -c CCL2 examples/*/*.lisp

prep-cvs:
	cd idlcomp; make prep-cvs
