CFLAGS = -std=c99 -pedantic -Wall -Wnodeprecated-declarations

p2: p2.c share.o
	tcc -o $@ ${CFLAGS} $^

p2.c: header.h

p1: p1.c share.o
	tcc -o $@ ${CFLAGS} $^

p1.c: header.h

share.o: share.c
	tcc -c ${CFLAGS} $^
