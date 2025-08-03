documentation: 
	cabal haddock --haddock-all
	rm -rf docs/*
	cp -R dist-newstyle/build/*/*/*/doc/html/seat-arrangement/* docs/
