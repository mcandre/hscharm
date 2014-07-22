FLAGS=-O2 -Wall -fwarn-tabs --make -fforce-recomp

all: test

test: hellocharm
	./hellocharm

hellocharm: HelloCharm.hs HsCharm.hs charm/charm.c charm/charm.h
	ghc $(FLAGS) -o hellocharm HelloCharm.hs HsCharm.hs charm/charm.c charm/charm.h

ddr: DDR.hs HsCharm.hs charm/charm.c charm/charm.h
	ghc $(FLAGS) -o ddr DDR.hs HsCharm.hs charm/charm.c charm/charm.h -package random-extras

rl: RL.hs HsCharm.hs charm/charm.c charm/charm.h
	ghc $(FLAGS) -o rl RL.hs HsCharm.hs charm/charm.c charm/charm.h -package random-extras -package base

hlint:
	hlint .

lint: hlint

churn:
	bundle exec churn

clean:
	-rm rl
	-rm ddr
	-rm hellocharm
	-rm *.exe
	-rm *.o
	-rm *.hi
