#include "header.h"

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

int
cmpint(const void *p1, const void *p2)
{
	int a, b;

	a = * (const int *) p1;
	b = * (const int *) p2;
	return (a < b) ? -1 : (a > b) ? 1 : 0;
}

