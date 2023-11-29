p2: p2.c share.o
	tcc -o $@ $^

p1: p1.c share.o
	tcc -o $@ $^

p1.c: header.h

p2.c: header.h

share.o: share.c
	tcc -c $^
