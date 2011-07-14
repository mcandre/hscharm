CCHARM=/usr/include/charm.c
HCHARM=/usr/include/charm.h

all: hellocharm

charm.c:
	cp $(CCHARM) .
	cp $(HCHARM) .

hellocharm: hellocharm.hs hscharm.hs charm.c
	ghc --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm.c charm.h

clean:
	-rm hellocharm
	-rm *.hi
	-rm *.o
	-rm charm.c
	-rm charm.h