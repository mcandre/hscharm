CCHARM=/usr/include/charm.c
HCHARM=/usr/include/charm.h
# ICHARM=-I/usr/include
# LCHARM=-dylib-install-name /usr/lib/libcharm.dynlib

all: hellocharm

charm.c:
	cp $(CCHARM) .
	cp $(HCHARM) .

hellocharm: hscharm.hs charm.c
	ghc --make -fforce-recomp -o hscharm hscharm.hs charm.c charm.h # $(ICHARM) $(LCHARM)

clean:
	-rm hellocharm
	-rm hscharm
	-rm *.hi
	-rm *.o
	-rm charm.c
	-rm charm.h