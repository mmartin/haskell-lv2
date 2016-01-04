all: clean amp.c Amp.hs
	ghc -c -O Amp.hs
	ghc --make -no-hs-main -optc-O -dynamic -shared -fPIC \
		init.c amp.c Amp -o amp.so -lHSrts-ghc7.8.4

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.hi
	rm -f *stub*
