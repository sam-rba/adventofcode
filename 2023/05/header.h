#define MAXLINE 256
#define MAPS 48
#define FNAME "/dev/stdin"

typedef unsigned long Seed;

struct map {
	Seed dst;
	Seed src;
	int len;
};

void readmap(struct map *mapp, const char line[]);
int isdigit(char c);

