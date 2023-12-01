#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

#define MAXLINE 256
#define MAXCOLOR 64

struct mapnode *parseinput(void);
int lgetline(char *line, int lim);
int isdigit(char c);
int getcolor(char *line, char *color);

int
main()
{
	struct mapnode *bags;
	struct bagnode *shiny;

	bags = parseinput();

	shiny = mapsearch(bags, "shiny gold");
	if (shiny == NULL)
		printf("no shiny bag\n");
	else
		printf("Part 1: %d\n", children(shiny)-1);

	mapfree(bags);

	return 0;
}

struct mapnode *
parseinput(void)
{
	struct mapnode *bags;
	struct bagnode *outer;
	struct bagnode *inner;
	char line[MAXLINE];
	char color[MAXCOLOR];
	int i, j;

	bags = NULL;
	while (lgetline(line, MAXLINE) > 0) {
		i = 0;
		while (strncmp(&line[i], " bags", 5) != 0) {
			color[i] = line[i++];
		}
		color[i] = '\0';

		outer = mapsearch(bags, color);
		if (outer == NULL)
			bags = mapadd(bags, outer = newbag(color));

		while (!isdigit(line[++i])) /* skip */
			;
		while ((j = getcolor(&line[i], color)) > 0) {
			i += j;

			inner = mapsearch(bags, color);
			if (inner == NULL)
				bags = mapadd(bags, inner = newbag(color));
			hold(outer, inner);

			/* skip */
			while (line[i] != '.' && !isdigit(line[++i]))
				;
			if (line[i] == '.')
				break;
		}
	}
	return bags;
}

int
lgetline(char *s, int lim)
{
	int i, c;

	i = 0;
	while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (lim == 0)
		printf("Line exceeded limit, truncating.\n");
	s[i] = '\0';
	return i;
}

int
isdigit(char c)
{
	return '1' <= c && c <= '9';
}

int
getcolor(char *line, char *color)
{
	int i, j;

	i = 0;
	while (isdigit(line[i++]))
		;
	j = 0;
	while (strncmp(&line[i], " bag", 4) != 0)
		color[j++] = line[i++];
	color[j] = '\0';
	return j;
}

