#include <stdlib.h>
#include <stdio.h>

#include "header.h"

int
main()
{
	FILE *file;
	char line[MAXLINE];
	/* TODO: use binary tree */
	int winning[MAXWINNING], nwinning;
	Stack *have;
	int i, num, cardpoints, totalpoints;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}
	have = NULL;
	totalpoints = 0;
	while (fgets(line, MAXLINE, file) != NULL) {
		/* skip `Card %d:` */
		for (i = 6; line[i] != ':'; i++)
			;

		nwinning = num = 0;
		/* get winning numbers */
		while (line[++i] != '|') {
			if (isdigit(line[i])) {
				num = (num * 10) + (line[i] - '0');
			} else if (nwinning < MAXWINNING) {
				if (num > 0)
					winning[nwinning++] = num;
				num = 0;
			} else {
				printf("MAXWINNING exceeded\n");
				fclose(file);
				return 1;
			}
		}
		/* get numbers we have */
		stfree(have);
		have = pushnums(have, line, ++i);

		/* check which winning numbers we have */
		qsort(winning, nwinning, sizeof(int), cmpint);
		cardpoints = 0;
		while (have != NULL) {
			have = stpop(have, &num);
			if (bsearch(&num, winning, nwinning, sizeof(int), cmpint)
					!= NULL) /* number we have is in winning set */
				cardpoints = (cardpoints == 0) ? 1 : cardpoints*2;
		}
		totalpoints += cardpoints;
	}

	printf("Part 1: %d\n", totalpoints);

	fclose(file);
	return 0;
}

