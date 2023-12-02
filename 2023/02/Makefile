include ../config.mk

all: p1 p2

p2: p2.o share.o
	tcc -o $@ ${CFLAGS} $^

p1: p1.o share.o
	tcc -o $@ ${CFLAGS} $^

%.o: %.c header.h
	tcc -c ${CFLAGS} $<

.PHONY: all
