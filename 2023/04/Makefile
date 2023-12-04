include ../config.mk

CC = tcc

all: p1 p2

p2: p2.o share.o stack.o btree.o
	${CC} -o $@ ${CFLAGS} $^

p1: p1.o share.o stack.o btree.o
	${CC} -o $@ ${CFLAGS} $^

p1.o: p1.c header.h btree.h
	${CC} -c ${CFLAGS} $<

p2.o: p2.c header.h btree.h
	${CC} -c ${CFLAGS} $<

stack.o: stack.c stack.h
	${CC} -c ${CFLAGS} $<

btree.o: btree.c btree.h
	${CC} -c ${CFLAGS} $<

share.o: share.c header.h
	${CC} -c ${CFLAGS} $<

.PHONY: all
