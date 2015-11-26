.PHONY: all run test conf init

all:
	cabal build
	cabal install

run:
	cabal run

test:
	cabal test

conf:
	DYLD_LIBRARY_PATH=/usr/local/opt/icu4c/lib                        \
        cabal install text-icu                                      \
            --extra-include-dirs=/usr/local/opt/icu4c/include       \
            --extra-lib-dirs=/usr/local/opt/icu4c/lib
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

init:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
