all: HSetHTML.cgi

%.cgi: %.hs
	ghc -package cgi -package xhtml --make -optl-static -optl-pthread -static -o $@ $< 2>/dev/null
	strip $@

clean:
	rm -f *.cgi *.o *.hi
