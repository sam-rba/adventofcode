p1: p1.c tree.o bag.o
	tcc -o $@ $^

p1.c: header.h

tree.o: tree.c
	tcc -c $^

tree.c: header.h

bag.o: bag.c
	tcc -c $^

bag.c: header.h
