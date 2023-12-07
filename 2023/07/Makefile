include ../config.mk

CC = tcc
LINK = ${CC} -o $@ $^

all: p1 p2

p1: p1.o share.o
	${LINK}

p2: p2.o share.o
	${LINK}

%.o: %.c header.h
	${CC} -c ${CFLAGS} $<

.PHONY: all
