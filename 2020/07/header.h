struct mapnode {
	struct bagnode *bag;
	struct mapnode *left;
	struct mapnode *right;
};

struct bagnode {
	char *color;
	int visited;
	int nholds;
	int nheldby;
	struct bagnode **holds;
	struct bagnode **heldby;
};

struct bagnode *newbag(char *color);
void bagfree(struct bagnode *node);
void hold(struct bagnode *outer, struct bagnode *inner);
int children(struct bagnode *node);

struct mapnode *mapadd(struct mapnode *node, struct bagnode *bag);
void mapfree(struct mapnode *node);
struct bagnode *mapsearch(struct mapnode *node, char *color);
void mapprint(struct mapnode *node);

