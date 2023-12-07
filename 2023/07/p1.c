#include <stdlib.h>
#include <stdio.h>

#include "header.h"

int
main()
{
	char line[MAXLINE];
	struct hand hands[MAXHANDS];
	int nhands;
	FILE *file;
	int i, winnings;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}

	/* parse input */
	nhands = 0;
	while (nhands < MAXHANDS && fgets(line, MAXLINE, file) != NULL)
		parsehand(&hands[nhands++], line);
	if (nhands == MAXHANDS) {
		printf("MAXHANDS exceeded\n");
		fclose(file);
		return 1;
	}

	qsort(hands, nhands, sizeof(struct hand), cmphand);

	winnings = 0;
	for (i = 0; i < nhands; i++)
		winnings += hands[i].bid * (i + 1);
	printf("Part 1: %d\n", winnings);

	fclose(file);
	return 0;
}

Card
parsecard(char c) {
	if ('2' <= c && c <= '9')
		return c - '2' + TWO;
	switch (c) {
	case 'T':
		return TEN;
	case 'J':
		return JACK;
	case 'Q':
		return QUEEN;
	case 'K':
		return KING;
	case 'A':
		return ACE;
	}
	printf("invalid card: %c\n", c);
	return -1;
}

HandType
handtype(struct hand *h)
{
	/* number of each type of card in hand */
	static int cardsoftype[NCARDTYPES];
	int i;

	for (i = 0; i < NCARDTYPES; i++)
		cardsoftype[i] = 0;

	for (i = 0; i < CARDS; i++)
		cardsoftype[h->cards[i]]++;

	/* most plentiful card type at index 0 */
	qsort(cardsoftype, NCARDTYPES, sizeof(int), rcmpint);

	switch (cardsoftype[0]) {
	case 5:
		return FIVE_OF_A_KIND;
	case 4:
		return FOUR_OF_A_KIND;
	case 3:
		return (cardsoftype[1] == 2) ? FULL_HOUSE : THREE_OF_A_KIND;
	case 2:
		return (cardsoftype[1] == 2) ? TWO_PAIR : PAIR;
	}
	return HIGH_CARD;
}

