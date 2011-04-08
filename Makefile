CCHARM=/usr/include/charm.c
ICHARM=-I/usr/include
LCHARM=-dylib-install-name /usr/lib/libcharm.dynlib

all: hellocharm

charm.c:
	cp $(CCHARM) .

hellocharm: hellocharm.hs charm.hs charm.c
	ghc --make -fforce-recomp -o hellocharm hellocharm.hs charm.hs charm.c $(ICHARM) $(LCHARM)

clean:
	-rm hellocharm
	-rm *.hi
	-rm *.o
	-rm charm.c