include ../config.mk

CC = tcc

p2: p2.o share.o
	${CC} -o $@ ${CFLAGS} $^

p1: p1.o share.o
	${CC} -o $@ ${CFLAGS} $^

%.o: %.c header.h
	${CC} -c ${CFLAGS} $<
