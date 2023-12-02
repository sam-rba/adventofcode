include ../config.mk

p2: p2.c share.o
	tcc -o $@ ${CFLAGS} $^

p2.c: header.h

p1: p1.c share.o
	tcc -o $@ ${CFLAGS} $^

p1.c: header.h

share.o: share.c
	tcc -c ${CFLAGS} $^

share.c: header.h
