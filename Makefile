default: build

setup: kindle-clippings.cabal Text
	ghc Setup.hs

build: setup 
	./Setup --user configure
	./Setup build 

install: build
	./Setup install

tests: install
	ghc tests/*.hs

test: tests
	./tests/Reader

clean: setup
	./Setup clean
	find . -executable -type f | xargs rm
	find . | grep -E '\.o|(hi)$$' | xargs rm
