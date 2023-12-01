#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

char ldigit(char *s);
char rdigit(char *s);
char digitword(char *s);

int
main()
{
	char line[MAXLINE];
	char l, r;
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
	printf("Part 2: %d\n", sum);

	return 0;
}

char
ldigit(char *s)
{
	int i;
	char c;

	for (i = 0; s[i] != '\0'; i++) {
		if (isdigit(s[i]))
			return s[i];
		if ((c = digitword(&s[i])) != '\0')
			return c;
	}
	return '\0';
}

char
rdigit(char *s)
{
	int i;
	char c;

	for (i = strlen(s)-1; i >= 0; i--) {
		if (isdigit(s[i]))
			return s[i];
		if ((c = digitword(&s[i])) != '\0')
			return c;
	}
	return '\0';
}

char
digitword(char *s)
{
	if (strncmp(s, "one", 3) == 0)
		return '1';
	if (strncmp(s, "two", 3) == 0)
		return '2';
	if (strncmp(s, "three", 5) == 0)
		return '3';
	if (strncmp(s, "four", 4) == 0)
		return '4';
	if (strncmp(s, "five", 4) == 0)
		return '5';
	if (strncmp(s, "six", 3) == 0)
		return '6';
	if (strncmp(s, "seven", 5) == 0)
		return '7';
	if (strncmp(s, "eight", 5) == 0)
		return '8';
	if (strncmp(s, "nine", 4) == 0)
		return '9';
	return '\0';
}

