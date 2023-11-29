#include <stdio.h>

#define ROWS 128
#define COLS 8

int
lgetline(char *s, int lim)
{
	int i, c;

	i = 0;
	while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
		s[i++] = c;
	s[i] = '\0';
	return i;
}

int
row(char *s)
{
	int i, lo, hi;

	i = 0;
	lo = 0;
	hi = ROWS-1;
	while (s[i] != '\0' && lo < hi)
		switch (s[i++]) {
		case 'F':
			hi = (lo+hi)/2;
			break;
		case 'B':
			lo = (hi+lo+1)/2;
			break;
		default:
			printf("Unrecognized char: %c\n", s[i-1]);
			return -1;
		}
	return (lo == hi) ? lo : -1;
}

int
col(char *s)
{
	int i, lo, hi;

	for (i = 0; s[i] != '\0'; i++)
		if (s[i] == 'L' || s[i] == 'R')
			break;
	lo = 0;
	hi = COLS-1;
	while (s[i] != '\0' && lo < hi)
		switch (s[i++]) {
		case 'L':
			hi = (lo+hi)/2;
			break;
		case 'R':
			lo = (hi+lo+1)/2;
			break;
		default:
			printf("Unrecognized char: %c\n", s[i-1]);
			return -1;
		}
	return (lo == hi) ? lo : -1;
}

int
id(int row, int col) { return row * 8 + col; }

