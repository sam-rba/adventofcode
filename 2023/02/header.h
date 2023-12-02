#define MAXLINE 256

struct Set {
	int r;
	int g;
	int b;
};

int lgetline(char *line, int lim);
int parseid(char *line, int *id);
int parseset(char *line, struct Set *set);
int isdigit(char c);

