FLAGS=-O2 -Wall -fwarn-tabs --make -fforce-recomp

all: test

test: bin/hellocharm
	bin/hellocharm

bin/hellocharm: HelloCharm.hs HsCharm.hs charm/charm.c charm/charm.h
	mkdir -p bin/
	ghc $(FLAGS) -o bin/hellocharm HelloCharm.hs HsCharm.hs charm/charm.c charm/charm.h

bin/ddr: DDR.hs HsCharm.hs charm/charm.c charm/charm.h
	mkdir -p bin/
	ghc $(FLAGS) -o bin/ddr DDR.hs HsCharm.hs charm/charm.c charm/charm.h -package random-extras

bin/rl: RL.hs HsCharm.hs charm/charm.c charm/charm.h
	mkdir -p bin/
	ghc $(FLAGS) -o bin/rl RL.hs HsCharm.hs charm/charm.c charm/charm.h -package random-extras -package base

hlint:
	hlint .

lili:
	bundle exec lili .

lint: hlint lili

churn:
	bundle exec churn

clean:
	-rm -rf bin/
	-rm *.o
	-rm *.hi
