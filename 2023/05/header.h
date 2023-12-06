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
int inwindow(unsigned long x, unsigned long winstart, int winlen);
Seed srcend(const struct map *m);
int isdigit(char c);

