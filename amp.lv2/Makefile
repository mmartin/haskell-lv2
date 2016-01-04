all: clean lv2_stub.c Amp.hs LV2.hs
	ghc -c -O2 -dynamic LV2.hs
	ghc -c -O2 -dynamic Amp.hs
	ghc Amp.o LV2.o lv2_stub.c -o amp.so \
		-no-hs-main -fPIC -dynamic -shared -lHSrts-ghc7.8.4 \
		-optc -O2 -package template-haskell

clean:
	rm -f *.o
	rm -f *.s
	rm -f a.out
	rm -f *.so
	rm -f *.hi
	rm -f *_stub.h
	rm -f *dyn*
