#include <stdlib.h>
#include <stdio.h>

#include "header.h"
#include "btree.h"

#define MAXCARDS 256

int
main()
{
	FILE *file;
	char line[MAXLINE];
	BTree *winning;
	Stack *have;
	int i, num, card, cards, nhavewinning;
	int copies[MAXCARDS];

	for (i = 0; i < MAXCARDS; i++)
		copies[i] = 0;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}
	winning = NULL;
	have = NULL;
	cards = 0;
	for (card = 1; fgets(line, MAXLINE, file) != NULL; card++) {
		/* skip `Card %d:` */
		for (i = 6; line[i] != ':'; i++)
			;

		/* get winning numbers */
		num = 0;
		btfree(&winning);
		while (line[++i] != '|') {
			if (isdigit(line[i])) {
				num = (num * 10) + (line[i] - '0');
			} else if (num > 0) {
				winning = btadd(winning, num);
				num = 0;
			}
		}
		/* get numbers we have */
		stfree(&have);
		have = pushnums(have, line, ++i);

		nhavewinning = 0; /* winning numbers we have */
		while (have != NULL) {
			have = stpop(have, &num);
			if (btcontains(winning, num)) {
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

	stfree(&have);
	btfree(&winning);
	fclose(file);
	return 0;
}

