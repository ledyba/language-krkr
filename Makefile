.PHONY: init all run conf

init:
	cabal sandbox init
	cabal install --only-dependencies

all:
	cabal build

run:
	cabal run

conf:
	cabal configure
