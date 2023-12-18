include ../config.mk

CC = tcc

p1: p1.o
	${CC} -o $@ $^

%.o: %.c
	${CC} -c ${CFLAGS} $<
