.PHONY: init all run test conf

init:
	cabal sandbox init
	cabal install --only-dependencies

all:
	cabal build

run:
	cabal run

test:
	cabal test

conf:
	cabal install --only-dependencies
	cabal configure
