#include <stdio.h>

#include "header.h"

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

int
rcmpint(const void *p1, const void *p2)
{
	int x, y;

	x = *(int *) p1;
	y = *(int *) p2;
	return (x > y) ? -1 : (x < y) ? 1 : 0;
}

