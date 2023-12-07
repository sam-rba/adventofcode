#include <math.h>

#include "header.h"

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

