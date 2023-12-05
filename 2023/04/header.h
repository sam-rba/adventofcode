#define FNAME "/dev/stdin"
#define MAXLINE 256

typedef struct btnode {
	int data;
	struct btnode *left;
	struct btnode *right;
} BTree;

typedef struct stnode {
	int data;
	struct stnode *next;
} Stack;

BTree *btadd(BTree *btr, int data);
int btcontains(BTree *btr, int data);
void btfree(BTree **btr);

Stack *stpush(Stack *stk, const int data);
Stack *stpop(Stack *stk, int *data);
void stfree(Stack **stk);

int isdigit(char c);
Stack *pushnums(Stack *stk, const char *str, int start);
BTree *addnums(BTree *btr, const char *str, int *i, char delim);

