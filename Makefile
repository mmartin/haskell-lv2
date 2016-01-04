all: clean lv2_stub.c LV2.hs
	ghc -c -O LV2.hs
	ghc --make -no-hs-main -optc-O -dynamic -shared -fPIC \
		lv2_stub.c LV2 -o amp.so -lHSrts-ghc7.8.4

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.hi
	rm -f *_stub.h
