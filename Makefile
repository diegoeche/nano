all:
	ghc -Wall --make *.hs
clean: 	
	rm *.hi & rm *.o & rm Main