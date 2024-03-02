include ../config.mk

CC = tcc

main: main.o queue.o
	${CC} -o $@ ${CFLAGS} $^

%.o: %.c header.h
	${CC} -c ${CFLAGS} $<
