SKEY = $(shell echo "$$RANDOM $$RANDOM $$RANDOM $$RANDOM" | md5sum | cut -d \  -f 1)

all: HSetHTML.cgi

%.cgi: %.hs
	ghc -D__SERVER_SECRET_KEY__=\"${SKEY}\" -package cgi -package xhtml --make -optl-static -optl-pthread -static -o $@ $< 2>/dev/null
	strip $@

clean:
	rm -f *.cgi *.o *.hi
