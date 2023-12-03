#include <string.h>

#include "header.h"

void
zero(char *line)
{
	int i;
	for (i = 0; i < MAXLINE-1; i++)
		line[i] = '.';
	line[MAXLINE-1] = '\0';
}

void
shiftwindow(char *line0, char *line1, char *line2)
{
	strcpy(line0, line1);
	strcpy(line1, line2);
}

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

