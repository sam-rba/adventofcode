#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

#define MAXLINKS 32

struct bagnode *bagalloc(void);

struct bagnode *
newbag(char *color)
{
	struct bagnode *node = bagalloc();
	node->color = strdup(color);
	node->visited = node->nholds = node->nheldby = 0;
	node->holds = (struct bagnode **) malloc(MAXLINKS * sizeof(struct bagnode *));
	node->heldby = (struct bagnode **) malloc(MAXLINKS * sizeof(struct bagnode *));
	return node;
}

void
bagfree(struct bagnode *node)
{
	free(node->holds);
	free(node->heldby);
}

void
hold(struct bagnode *outer, struct bagnode *inner)
{
	if (outer->nholds == MAXLINKS-1 || inner->nheldby == MAXLINKS-1) {
		printf("MAXLINKS exceeded.\n");
	} else {
		outer->holds[outer->nholds++] = inner;
		inner->heldby[inner->nheldby++] = outer;
	}
}

int
children(struct bagnode *node)
{
	int i, count;

	if (node->visited)
		return 0;

	count = node->visited = 1;
	for (i = 0; i < node->nheldby; i++) {
		count += children(node->heldby[i]);
	}
	return count;
}

struct bagnode *
bagalloc(void)
{
	return (struct bagnode *) malloc(sizeof(struct bagnode));
}

