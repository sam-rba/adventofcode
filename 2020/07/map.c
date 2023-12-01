#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

struct mapnode * mapalloc(void);

struct mapnode *
mapadd(struct mapnode *node, struct bagnode *bag)
{
	int cond;

	if (node == NULL) {
		node = mapalloc();
		node->bag = bag;
		node->left = node->right = NULL;
	} else if ((cond = strcmp(bag->color, node->bag->color)) == 0) {
		printf("%s already in tree, not adding.\n", bag->color);
	} else if (cond < 0) {
		node->left = mapadd(node->left, bag);
	} else {
		node-> right = mapadd(node->right, bag);
	}
	return node;
}

void
mapfree(struct mapnode *node)
{
	if (node == NULL)
		return;
	mapfree(node->left);
	mapfree(node->right);
	bagfree(node->bag);
}

struct bagnode *
mapsearch(struct mapnode *node, char *color)
{
	int cond;

	if (node ==  NULL)
		return NULL;

	if ((cond = strcmp(color, node->bag->color)) == 0)
		return node->bag;
	else if (cond < 0)
		return mapsearch(node->left, color);
	else
		return mapsearch(node->right, color);
}

void
mapprint(struct mapnode *node)
{
	if (node == NULL)
		return;
	mapprint(node->left);
	printf("%s\n", node->bag->color);
	mapprint(node->right);
}

struct mapnode *
mapalloc(void)
{
	return (struct mapnode *) malloc(sizeof(struct mapnode));
}

