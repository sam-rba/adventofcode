CC = gcc
CFLAGS = -std=c99 -Wall -pedantic -Wno-deprecated-declarations -g

p1: p1.o
	${CC} -o $@ $^

%.o: %.c
	${CC} -c ${CFLAGS} $<

clean:
	rm -f ./*.o
	rm -f p1 p2