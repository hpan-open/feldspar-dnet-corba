
all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.bak

tomac:
	tomac *.lisp *.system TODO
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp *.system TODO

frommac:
	frommac *.lisp *.system TODO
