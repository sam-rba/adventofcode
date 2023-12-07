include ../config.mk

CC = tcc
LDFLAGS = -lm
LINK = ${CC} -o $@ ${LDFLAGS} $^

all: p1 p2

p2: p2.o share.o
	${LINK}

p1: p1.o share.o
	${LINK}

%.o: %.c header.h
	${CC} -c ${CFLAGS} $<

.PHONY: all
