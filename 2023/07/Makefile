include ../config.mk

CC = tcc
LINK = ${CC} -o $@ $^

p1: p1.o
	${LINK}

%.o: %.c
	${CC} -c ${CFLAGS} $<
