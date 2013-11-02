all: test

test: hellocharm
	./hellocharm

hellocharm: hellocharm.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm.c charm.h -Wall

ddr: ddr.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o ddr ddr.hs hscharm.hs charm.c charm.h -package random-extras -Wall

rl: rl.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o rl rl.hs hscharm.hs charm.c charm.h -package random-extras -package base -Wall

lint:
	hlint .

clean:
	-rm rl
	-rm ddr
	-rm hellocharm
	-rm *.hi
	-rm *.o
