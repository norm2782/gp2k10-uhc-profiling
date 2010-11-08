default:
	make clean && make install

install:
	cabal install

clean:
	find ./src -iname "*.grin" -o -iname "*.core*" -o -iname "*.hi" -o -iname "*.[c|o]" | xargs rm
	cabal clean
