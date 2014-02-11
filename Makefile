all: test

test: hellocharm
	./hellocharm

hellocharm: hellocharm.hs hscharm.hs charm/charm.c charm/charm.h
	ghc -O2 -Wall --make -fforce-recomp -o hellocharm hellocharm.hs hscharm.hs charm/charm.c charm/charm.h

ddr: ddr.hs hscharm.hs charm/charm.c charm/charm.h
	ghc -O2 -Wall --make -fforce-recomp -o ddr ddr.hs hscharm.hs charm/charm.c charm/charm.h -package random-extras

rl: rl.hs hscharm.hs charm/charm.c charm/charm.h
	ghc -O2 -Wall --make -fforce-recomp -o rl rl.hs hscharm.hs charm/charm.c charm/charm.h -package random-extras -package base

lint:
	hlint .

churn:
	bundle exec churn

clean:
	-rm *.exe
	-rm *.o
	-rm *.hi
	-rm rl
	-rm ddr
	-rm hellocharm

