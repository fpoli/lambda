SHELL := /bin/bash

.PHONY: all deps test run lint clean

all: deps build

deps:
	cabal install --only-dependencies --enable-tests

build:
	cabal build --ghc-options=$(GHC_OPTS)

test:
	cabal test --show-details=always --ghc-options=$(GHC_OPTS)

run:
	./dist/build/lambda/lambda

lint:
	hlint . --ignore="Eta reduce" --ignore="Reduce duplication"

clean:
	rm -f *.o   */*.o   */*/*.o 
	rm -f *.hi  */*.hi  */*/*.hi
	rm -f *.tix */*.tix */*/*.tix
	cabal clean
