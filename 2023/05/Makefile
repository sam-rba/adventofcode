include ../config.mk

CC = tcc
LINK = ${CC} -o $@ $^
COMP = ${CC} -c ${CFLAGS} $<

all: p2 p1

clean:
	rm ./p[12] ./*.o

p1: p1.o share.o
	${LINK}

p2: p2.o share.o
	${LINK}

%.o: %.c header.h
	${COMP}

.PHONY: all clean
