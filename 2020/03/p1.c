#include <stdio.h>

#include "share.h"

#define MAXCOL 64 /* maximum width (# of columns) of tree map */
#define INITX 0 /* initial x coordinate */
#define DX 3 /* x component of slope */
#define DY 1 /* y component of slope */
#define TREE '#'

int strlen(char *s);
int lgetlines(char *line, int lim, int nlines);

int
main()
{
	int x, cols, trees;
	char row[MAXCOL];

	trees = 0;
	for (x = INITX; (cols = lgetlines(row, MAXCOL, DY)) > 0; x += DX) {
		if (x >= cols)
			x -= cols;
		if (row[x] == TREE)
			trees++;
	}
	printf("Part 1: %d\n", trees);
	return 0;
}

int
strlen(char *s)
{
	int i;
	i = 0;
	while (s[i++] != '\0')
		;
	return --i;
}

int
lgetlines(char *line, int lim, int nlines)
{
	while (nlines-- > 1)
		(void) lgetline(line, lim);
	return lgetline(line, lim);
}

