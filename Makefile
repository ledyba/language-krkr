.PHONY: atom test

build:
	stack build

test:
	stack test

atom:
	stack exec atom .
