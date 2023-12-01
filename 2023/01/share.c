#include <stdio.h>

#include "header.h"

int
lgetline(char *s, int lim)
{
	int i, c;

	i = 0;
	while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (lim == 0)
		printf("Line exceeded limit, truncating\n");
	s[i] = '\0';
	return i;
}

int
isdigit(char c) { return '1' <= c && c <= '9'; }

