.SUFFIXES: .hs .hi .o .x

TESTS=Symmath

.hs.x:
	ghc -i.. -o $@ $<

Symmath : TestSymmath.x
	./TestSymmath.x

clean:
	rm -f *.hi *.o *.x \
	rm -f ../Symmath/*.hi ../Symmath/*.o
