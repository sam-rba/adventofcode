include ../config.mk

CC = tcc

all: p1 p2

p1: p1.o share.o stack.o
	${CC} -o $@ $^

p2: p2.o share.o stack.o
	${CC} -o $@ $^

%.o: %.c header.h
	${CC} ${CFLAGS} -c $<

.PHONY: all
