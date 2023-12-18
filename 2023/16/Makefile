include ../config.mk

CC = tcc

p1: p1.o stack.o
	${CC} -o $@ $^

%.o: %.c header.h
	${CC} -c ${CFLAGS} $<
