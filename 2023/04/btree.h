struct btnode {
	int data;
	struct btnode *left;
	struct btnode *right;
};
typedef struct btnode BTree;

BTree *btadd(BTree *bt, int data);
int btcontains(BTree *bt, int data);
void btfree(BTree *bt);

