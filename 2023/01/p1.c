#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 64

int lgetline(char *s, int lim);
char ldigit(char *s);
char rdigit(char *s);
int isdigit(char c);

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

int
isdigit(char c) { return '1' <= c && c <= '9'; }

