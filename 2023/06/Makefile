include ../config.mk

CC = tcc
LDFLAGS = -lm

p1: p1.o
	${CC} -o $@ ${LDFLAGS} $^

%.o: %.c
	${CC} -c ${CFLAGS} $<
