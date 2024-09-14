CC = gcc
CFLAGS = -std=c99 -Wall -pedantic -Wno-deprecated-declarations -g

all: p1 p2

p1: p1.o lib.o
	${CC} -o $@ $^

p2: p2.o lib.o
	${CC} -o $@ $^

%.o: %.c header.h
	${CC} -c ${CFLAGS} $<

clean:
	rm -f ./*.o
	rm -f p1 p2