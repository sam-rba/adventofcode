#include <stdio.h>
#include <math.h>

#define RACES 4

typedef double Time;
typedef double Distance;

int waystowin(Time race, Distance record);

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

int
waystowin(Time race, Distance record)
{
	double root;
	int upper, lower;

	root = sqrt(race*race - 4*record);
	upper = ceil((race + root) / 2.0) - 1;
	lower = floor((race - root) / 2.0) + 1;
	return upper - lower + 1;
}

