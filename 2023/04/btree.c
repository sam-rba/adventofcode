#include <stdlib.h>

#include "btree.h"

BTree *
btadd(BTree *bt, int data)
{
	if (bt == NULL) {
		bt = (struct btnode *) malloc(sizeof(struct btnode));
		bt->data = data;
		bt->left = bt->right = NULL;
	}

	if (data < bt->data)
		bt->left = btadd(bt->left, data);
	else if (data > bt->data)
		bt->right = btadd(bt->right, data);

	return bt;
}

int
btcontains(BTree *bt, int data)
{
	if (bt == NULL)
		return 0;
	if (data == bt->data)
		return 1;
	if (data < bt->data)
		return btcontains(bt->left, data);
	return btcontains(bt->right, data);
}

void
btfree(BTree **bt)
{
	if (*bt == NULL)
		return;
	btfree(&((*bt)->left));
	btfree(&((*bt)->right));
	free(*bt);
	*bt = NULL;
}

