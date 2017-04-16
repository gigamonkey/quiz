export PATH

source := quiz.hs

all:
	ghc $(source)

run: all
	./quiz

lint:
	/Users/peter/Library/Haskell/bin/hlint $(source)

tidy:
	rm -f *~

clean: tidy
	rm -f quiz quiz.dat
