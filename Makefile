
all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err

tomac:
	tomac *.lisp *.system
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp *.system

frommac:
	frommac *.lisp *.system
