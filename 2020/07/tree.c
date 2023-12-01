#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"

struct tnode *
tadd(struct tnode *node, struct bagnode *bag)
{
	int cond;

	if (node == NULL) {
		node = (struct tnode *) malloc(sizeof(struct tnode));
		node->bag = bag;
		node->left = node->right = NULL;
	} else if ((cond = strcmp(bag->color, node->bag->color)) == 0) {
		printf("%s already in tree, not adding.\n", bag->color);
	} else if (cond < 0) {
		node->left = tadd(node->left, bag);
	} else {
		node-> right = tadd(node->right, bag);
	}
	return node;
}

void
tfree(struct tnode *node)
{
	if (node == NULL)
		return;
	tfree(node->left);
	tfree(node->right);
	bagfree(node->bag);
}

struct bagnode *
tsearch(struct tnode *node, char *color)
{
	int cond;

	if (node ==  NULL)
		return NULL;

	if ((cond = strcmp(color, node->bag->color)) == 0)
		return node->bag;
	else if (cond < 0)
		return tsearch(node->left, color);
	else
		return tsearch(node->right, color);
}

void
tprint(struct tnode *node)
{
	if (node == NULL)
		return;
	tprint(node->left);
	printf("%s\n", node->bag->color);
	tprint(node->right);
}

