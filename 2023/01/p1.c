#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

char ldigit(char *s);
char rdigit(char *s);

int
main()
{
	char line[MAXLINE];
	char l, r; /* left and right digits */
	int sum;

	sum = 0;
	while (lgetline(line, MAXLINE) > 0) {
		l = ldigit(line);
		if (l == '\0') {
			printf("no left digit.\n");
			break;
		}
		r = rdigit(line);
		if (sprintf(line, "%c%c", l, r) < 0) {
			printf("Error formatting digits\n");
			break;
		}
		sum += atoi(line);
	}
	printf("Part 1: %d\n", sum);

	return 0;
}

char
ldigit(char *s)
{
	int i;
	for (i = 0; s[i] != '\0'; i++)
		if (isdigit(s[i]))
			return s[i];
	return '\0';
}

char
rdigit(char *s)
{
	int i;
	for (i = strlen(s)-1; i >= 0; i--)
		if (isdigit(s[i]))
			return s[i];
	return '\0';
}

