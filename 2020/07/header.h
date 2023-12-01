struct tnode {
	struct bagnode *bag;
	struct tnode *left;
	struct tnode *right;
};

struct bagnode {
	char *color;
	int visited;
	int ncontainers;
	struct bagnode **containers;
};

struct bagnode *newbag(char *color);
void bagfree(struct bagnode *bag);
void addcontainer(struct bagnode *inner, struct bagnode *outer);
int ncontainers(struct bagnode *bag);

struct tnode *tadd(struct tnode *node, struct bagnode *bag);
void tfree(struct tnode *node);
struct bagnode *tsearch(struct tnode *node, char *color);
void tprint(struct tnode *node);

