SKEY = $(shell echo "$$RANDOM $$RANDOM $$RANDOM $$RANDOM" | md5sum | cut -d \  -f 1)
IMGDIR = ./cards/
IMGURL = /~kwantam/images/cards/

all: build/HSetHTML.cgi

cards: build/HSetSVGWrite
	mkdir -p $(IMGDIR)
	build/HSetSVGWrite $(IMGDIR)
	cd $(IMGDIR) && (for i in *; do convert $$i $$(basename $$i svg)png; done)
	rm $(IMGDIR)/*.svg

build/%.cgi: %.hs
	mkdir -p build
	ghc -D__IMGDIR__=\"${IMGURL}\" -D__SERVER_SECRET_KEY__=\"${SKEY}\" -package cgi -package xhtml --make -optl-static -optl-pthread -static -outputdir build -o $@ $< 2>/dev/null
	strip $@

build/% : %.hs
	mkdir -p build
	ghc --make -outputdir build -o $@ $<

images : HSetSVGWrite.hs

clean:
	rm -rf build $(IMGDIR)
