#include <stdio.h>

#include "header.h"

int
main()
{
	double time, distance;

	time = 45977295;
	distance = 305106211101695;
	/*
	time = 71530;
	distance = 940200;
	*/
	printf("Part 2: %d\n", waystowin(time, distance));
}

