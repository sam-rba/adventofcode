#include <stdio.h>

#include "header.h"

#define RACES 4

int
main()
{
	int times[RACES] =   {  45,   97,   72,   95 };
	int records[RACES] = { 305, 1062, 1110, 1695 };
	int i, prod;

	prod = 1;
	for (i = 0; i < RACES; i++)
		prod *= waystowin(times[i], records[i]);
	printf("Part 1: %d\n", prod);
	return 0;
}

