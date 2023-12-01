p1: p1.c map.o bag.o
	tcc -o $@ $^

p1.c: header.h

map.o: map.c
	tcc -c $^

map.c: header.h

bag.o: bag.c
	tcc -c $^

bag.c: header.h
