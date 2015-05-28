.PHONY: all run test conf init

all:
	cabal build
	cabal install

run:
	cabal run

test:
	cabal install --enable-tests
	cabal test

conf:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

init:
	cabal sandbox init
	cabal install --only-dependencies
