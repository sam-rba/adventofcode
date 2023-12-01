#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

#define MAXLINKS 32

struct bagnode *
newbag(char *color)
{
	struct bagnode *bag = (struct bagnode *) malloc(sizeof(struct bagnode));
	bag->color = strdup(color);
	bag->visited = bag->ncontainers = 0;
	bag->containers = (struct bagnode **) calloc(MAXLINKS, sizeof(struct bagnode *));
	return bag;
}

void
bagfree(struct bagnode *bag)
{
	free(bag->color);
	free(bag->containers);
	free(bag);
}

void
addcontainer(struct bagnode *inner, struct bagnode *outer)
{
	if (inner->ncontainers == MAXLINKS-1) {
		printf("MAXLINKS exceeded\n");
		return;
	}
	inner->containers[inner->ncontainers++] = outer;
}

int
ncontainers(struct bagnode *bag)
{
	if (bag->visited)
		return 0;

	int i, count;
	count = bag->visited = 1;
	for (i = 0; i < bag->ncontainers; i++) {
		count += ncontainers(bag->containers[i]);
	}
	return count;
}

