#include <stdlib.h>
#include <stdio.h>

#include "header.h"
#include "stack.h"

#define MAXCARDS 256

int
main()
{
	FILE *file;
	char line[MAXLINE];
	int winning[MAXWINNING], nwinning;
	Stack *have;
	int i, num, card, cards, nhavewinning;
	int copies[MAXCARDS];

	for (i = 0; i < MAXCARDS; i++)
		copies[i] = 0;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}
	have = NULL;
	cards = 0;
	for (card = 1; fgets(line, MAXLINE, file) != NULL; card++) {
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
		while (line[++i] != '\0') {
			if (isdigit(line[i])) {
				num = (num * 10) + (line[i] - '0');
			} else if (num > 0) {
				have = stpush(have, num);
				num = 0;
			}
		}

		/* check how many winning numbers we have */
		qsort(winning, nwinning, sizeof(int), cmpint);
		nhavewinning = 0; /* winning numbers we have */
		while (have != NULL) {
			have =stpop(have, &num);
			if (bsearch(&num, winning, nwinning, sizeof(int), cmpint)
					!= NULL) { /* number we have is in winning set */
				if (card + ++nhavewinning >= MAXCARDS) {
					printf("MAXCARDS exceeded\n");
					fclose(file);
					return 1;
				} else {
					copies[card + nhavewinning] += copies[card] + 1;
				}
			}
		}

		cards += copies[card] + 1;
	}


	printf("Part 2: %d\n", cards);

	fclose(file);
	return 0;
}

