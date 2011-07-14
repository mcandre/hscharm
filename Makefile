all: hellocharm

hellocharm: hellocharm.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm.c charm.h

clean:
	-rm hellocharm
	-rm *.hi
	-rm *.o