.PHONY: all conf

all:
	cabal build
	

conf:
	cabal configure
