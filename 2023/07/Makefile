include ../config.mk

CC = tcc
LINK = ${CC} -o $@ $^

all: p1 p2

p1: p1.o
	${LINK}

p2: p2.o
	${LINK}

%.o: %.c
	${CC} -c ${CFLAGS} $<

.PHONY: all
