#include <stdlib.h>
#include <stdio.h>

#include "header.h"

#define MAXCARDS 256

int
main()
{
	FILE *file;
	char line[MAXLINE];
	int winning[MAXWINNING], have[MAXHAVE];
	int nwinning, nhave;
	int i, num, card, cards;
	int copies[MAXCARDS];

	for (i = 0; i < MAXCARDS; i++)
		copies[i] = 0;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}

	cards = 0;
	for (card = 1; fgets(line, MAXLINE, file) != NULL; card++) {
		/* skip `Card %d:` */
		for (i = 6; line[i] != ':'; i++)
			;

		nwinning = nhave = num = 0;
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
		while (line[++i] != '\0') {
			if (isdigit(line[i])) {
				num = (num * 10) + (line[i] - '0');
			} else if (nhave < MAXHAVE) {
				if (num > 0)
					have[nhave++] = num;
				num = 0;
			} else {
				printf("MAXHAVE exceeded\n");
				fclose(file);
				return 1;
			}
		}

		/* check how many winning numbers we have */
		qsort(winning, nwinning, sizeof(int), cmpint);
		num = 0; /* winning numbers we have */
		while (--nhave >= 0) {
			if (bsearch(&have[nhave], winning, nwinning, sizeof(int), cmpint)
					!= NULL) { /* number we have is in winning set */
				if (card + ++num >= MAXCARDS) {
					printf("MAXCARDS exceeded\n");
					fclose(file);
					return 1;
				} else {
					copies[card + num] += copies[card] + 1;
				}
			}
		}

		cards += copies[card] + 1;
	}


	printf("Part 2: %d\n", cards);

	fclose(file);
	return 0;
}

