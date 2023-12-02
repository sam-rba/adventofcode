#include <stdio.h>

#include "header.h"

int power(struct Set *set);

int
main()
{
	char line[MAXLINE];
	int i, j, sum;
	struct Set set, min;

	sum = 0;
	while (lgetline(line, MAXLINE) > 0) {
		/* skip game id */
		i = 5;
		while (isdigit(line[i++]))
			;

		min.r = min.g = min.b = 0;
		for (; (j = parseset(&line[i], &set)) > 0; i += j) {
			if (set.r > min.r)
				min.r = set.r;
			if (set.g > min.g)
				min.g = set.g;
			if (set.b > min.b)
				min.b = set.b;
		}
		sum += power(&min);
	}
	printf("Part 2: %d\n", sum);
	return 0;
}

int
power(struct Set *set) { return set->r * set->g * set->b; }

