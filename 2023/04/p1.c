#include <stdlib.h>
#include <stdio.h>

#include "header.h"
#include "btree.h"

int
main()
{
	FILE *file;
	char line[MAXLINE];
	BTree *winning;
	Stack *have;
	int i, num, cardpoints, totalpoints;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}
	winning = NULL;
	have = NULL;
	totalpoints = 0;
	while (fgets(line, MAXLINE, file) != NULL) {
		/* skip `Card %d:` */
		for (i = 6; line[i] != ':'; i++)
			;

		/* get winning numbers */
		num = 0;
		btfree(winning);
		winning = NULL;
		while (line[++i] != '|') {
			if (isdigit(line[i])) {
				num = (num * 10) + (line[i] - '0');
			} else if (num > 0) {
				winning = btadd(winning, num);
				num = 0;
			}
		}
		/* get numbers we have */
		stfree(have);
		have = NULL;
		have = pushnums(have, line, ++i);

		/* check which winning numbers we have */
		cardpoints = 0;
		while (have != NULL) {
			have = stpop(have, &num);
			if (btcontains(winning, num))
				cardpoints = (cardpoints == 0) ? 1 : cardpoints*2;
		}
		totalpoints += cardpoints;
	}

	printf("Part 1: %d\n", totalpoints);

	stfree(have);
	btfree(winning);
	fclose(file);
	return 0;
}

