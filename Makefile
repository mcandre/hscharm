all: test

test: hellocharm
	./hellocharm

hellocharm: hellocharm.hs hscharm.hs charm.c charm.h
	ghc -O2 -Wall --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm.c charm.h

ddr: ddr.hs hscharm.hs charm.c charm.h
	ghc -O2 -Wall --make -fforce-recomp -o ddr ddr.hs hscharm.hs charm.c charm.h -package random-extras

rl: rl.hs hscharm.hs charm.c charm.h
	ghc -O2 -Wall --make -fforce-recomp -o rl rl.hs hscharm.hs charm.c charm.h -package random-extras -package base

lint:
	hlint .
	splint *.c *.h -preproc

clean:
	-rm *.exe
	-rm *.o
	-rm *.hi
	-rm rl
	-rm ddr
	-rm hellocharm

