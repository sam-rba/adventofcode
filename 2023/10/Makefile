include ../config.mk

CC = tcc
LINK = ${CC} -o $@ $^
COMP = ${CC} -c ${CFLAGS} $<

p1: p1.o
	${LINK}

%.o: %.c header.h
	${COMP}
