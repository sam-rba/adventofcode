#include <stdio.h>

#include "header.h"

/* total number of cubes of each color in the bag */
#define RED 12
#define GREEN 13
#define BLUE 14

int
main()
{
	int i, j, id, sum, possible;
	struct Set set;
	char line[MAXLINE];

	sum = 0;
	while (lgetline(line, MAXLINE) > 0) {
		i = parseid(line, &id);

		possible = 1;
		for (; (j = parseset(&line[i], &set)) > 0; i += j) {
			if (set.r > RED || set.g > GREEN || set.b > BLUE) {
				possible = 0;
				break;
			}
		}
		if (possible)
			sum += id;
	}
	printf("Part 1: %d\n", sum);
	return 0;
}

