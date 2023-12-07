#include <stdlib.h>
#include <stdio.h>

#define CARDS 5 /* cards per hand */
#define NCARDTYPES 13

#define FNAME "/dev/stdin"
#define MAXLINE 16
#define MAXHANDS 1024

typedef enum {
	TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN,
	JACK, QUEEN, KING, ACE
} Card;

typedef enum {
	HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND,
	FULL_HOUSE, FOUR_OF_A_KIND, FIVE_OF_A_KIND
} HandType;

struct hand {
	Card cards[CARDS];
	int bid;
};

void parsehand(struct hand *h, const char line[]);
Card parsecard(char c);
int isdigit(char c);
int cmphand(const void *p1, const void *p2);
HandType handtype(struct hand *h);
int rcmpint(const void *p1, const void *p2);

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

void
parsehand(struct hand *h, const char line[])
{
	int i;

	for (i = 0; i < CARDS; i++)
		h->cards[i] = parsecard(line[i]);
	h->bid = 0;
	while (isdigit(line[++i]))
		h->bid = (h->bid * 10) + (line[i] - '0');
}

Card
parsecard(char c) {
	if ('2' <= c && c <= '9')
		return c - '2';
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

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

int
cmphand(const void *p1, const void *p2)
{
	struct hand *h1, *h2;
	HandType t1, t2;
	int i;

	h1 = (struct hand *) p1;
	h2 = (struct hand *) p2;
	t1 = handtype(h1);
	t2 = handtype(h2);

	if (t1 < t2)
		return -1;
	if (t1 > t2)
		return 1;

	for (i = 0; i < CARDS; i++) {
		if (h1->cards[i] < h2->cards[i])
			return -1;
		if (h1->cards[i] > h2->cards[i])
			return 1;
	}
	printf("hands are equal\n");
	return 0;

}

HandType
handtype(struct hand *h)
{
	/* number of each type of card in hand */
	static int ncards[NCARDTYPES];
	int i;

	for (i = 0; i < NCARDTYPES; i++)
		ncards[i] = 0;
	for (i = 0; i < CARDS; i++)
		ncards[h->cards[i]]++;
	qsort(ncards, NCARDTYPES, sizeof(int), rcmpint);

	switch (ncards[0]) {
	case 5:
		return FIVE_OF_A_KIND;
	case 4:
		return FOUR_OF_A_KIND;
	case 3:
		return (ncards[1] == 2) ? FULL_HOUSE : THREE_OF_A_KIND;
	case 2:
		return (ncards[1] == 2) ? TWO_PAIR : PAIR;
	}
	return HIGH_CARD;
}

int
rcmpint(const void *p1, const void *p2)
{
	int x, y;

	x = *(int *) p1;
	y = *(int *) p2;
	return (x > y) ? -1 : (x < y) ? 1 : 0;
}

