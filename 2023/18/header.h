#include <stdio.h>

typedef struct dig {
	char dir;
	unsigned int dist;
	struct dig *next;
} Dig;

typedef struct point {
	int x, y;
	struct point *next;
} Point;

void digfree(Dig *d);

int push(Point **p, int x, int y);
void pointfree(Point *p);

Dig *parse(FILE *stream);
int parseline(const char *line, char *dir, unsigned int *dist);
Point *corners(Dig *digs);
unsigned long long area(Point *points);

