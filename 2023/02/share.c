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
parseid(char *line, int *id)
{
	int i;

	/* Skip to first digit */
	i = 5; /* number of chars in "Game " */

	*id = 0;
	while (isdigit(line[i])) {
		*id *= 10;
		*id += line[i++]-'0';
	}

	return ++i;
}

int
parseset(char *line, struct Set *set)
{
	int i, n;

	if (line[0] == '\0')
		return -1;

	i = set->r = set->g = set->b = 0;
	while (1) {
		n = 0;

		while (isdigit(line[++i])) {
			n *= 10;
			n += line[i]-'0';
		}

		switch (line[++i]) {
		case 'r':
			set->r = n;
			break;
		case 'g':
			set->g = n;
			break;
		case 'b':
			set->b = n;
			break;
		default:
			printf("unrecognized color: %c\n", line[i]);
			return -1;
		}

		while (line[i] != '\0' && line[i] != ',' && line[i] != ';')
			i++;
		if (line[i] == '\0' || line[i++] == ';')
			break;
	}

	return i;
}


int
isdigit(char c) { return '0' <= c && c <= '9'; }

