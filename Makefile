all: hellocharm

hellocharm: hellocharm.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm.c charm.h

ddr: ddr.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o ddr ddr.hs hscharm.hs charm.c charm.h -package random-extras

rl: rl.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o rl rl.hs hscharm.hs charm.c charm.h -package random-extras -package base

clean:
	-rm rl
	-rm ddr
	-rm hellocharm
	-rm *.hi
	-rm *.o
