.SUFFIXES: .hs .hi .o .x

TESTS=simplify

.hs.x:
	ghc -i.. -o $@ $<

simplify : TestSimplify.x
	./TestSimplify.x

clean:
	rm -f *.hi *.o *.x \
	rm -f ../Symmath/*.hi ../Symmath/*.o
