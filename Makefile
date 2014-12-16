SHELL := /bin/bash

.PHONY: all deps test run lint clean

all: deps build

deps:
	cabal install --only-dependencies --enable-tests

build:
	cabal build

test:
	cabal test --show-details=always

run:
	./dist/build/lambda/lambda

lint:
	hlint . --ignore="Eta reduce"

clean:
	rm -f *.o   */*.o   */*/*.o 
	rm -f *.hi  */*.hi  */*/*.hi
	rm -f *.tix */*.tix */*/*.tix
	cabal clean
