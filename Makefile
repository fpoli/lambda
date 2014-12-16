.PHONY = all run test lint clean
SHELL = bash

all:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	cabal build

run:
	./dist/build/lambda/lambda

test:
	cabal test --show-details=always

lint:
	hlint . --ignore="Eta reduce"

clean:
	rm -f *.o   */*.o   */*/*.o 
	rm -f *.hi  */*.hi  */*/*.hi
	rm -f *.tix */*.tix */*/*.tix
	cabal clean
