SHELL = /bin/bash

IN_PACKAGE = ./packages/rbk/

CONFIG = heuler-rbk.conf
SOURCES = $(wildcard */*.hs)


all: $(SOURCES) dist

dist: $($(IN_PACKAGE)$(CONFIG))
	cd $(IN_PACKAGE) ; cabal install 

.PHONY: clean
clean:
	rm -rf $(join $(IN_PACKAGE),dist)
	#ghc-pkg unregister heuler-rbk
