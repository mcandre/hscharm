all: hellocharm

hellocharm: hellocharm.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm.c charm.h

ddr: ddr.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o ddr ddr.hs hscharm.hs charm.c charm.h

clean:
	-rm ddr
	-rm hellocharm
	-rm *.hi
	-rm *.o