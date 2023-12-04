include ../config.mk

CC = tcc

all: p1 p2

p2: p2.o share.o
	${CC} -o $@ ${CFLAGS} $^

p1: p1.o share.o stack.o
	${CC} -o $@ ${CFLAGS} $^

p1.o: p1.c header.h stack.h
	${CC} -c ${CFLAGS} $<

stack.o: stack.c stack.h
	${CC} -c ${CFLAGS} $<

.PHONY: all
