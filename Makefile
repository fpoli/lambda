GHC_OPTS = -W -Wall -fno-warn-unused-do-bind -ferror-spans -O
.PHONY = default deps run test lint clean

default: main

deps:
	cabal install hunit parsec

run: main
	./main

main: *.hs
	ghc $(GHC_OPTS) --make main

test:
	@for file in Test*.hs */Test*.hs; do \
		echo "(*) Testing $$file ..."; \
		runhaskell $$file; \
	done

lint:
	hlint . --ignore="Eta reduce"

clean:
	rm -f *.o */*.o *.hi */*.hi *.tix */*.tix main
