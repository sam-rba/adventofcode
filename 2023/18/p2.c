#include "header.h"

char hexdir(char hex);

int
main() {
	Dig *digs;
	Point *points;

	digs = parse(stdin);
	points = corners(digs);
	digfree(digs);
	printf("gold: %llu\n", area(points));
	pointfree(points);
}

int
parseline(const char *line, char *dir, unsigned int *dist) {
	char dirhex;

	if (sscanf(line, "%*c %*u (#%5x%c", dist, &dirhex) != 2) {
		fprintf(stderr, "invalid input: %s", line);
		return 1;
	}
	if ((*dir = hexdir(dirhex)) == 0) {
		fprintf(stderr, "invalid direction: %c\n", dirhex);
		return 1;
	}
	return 0;
}

char
hexdir(char hex) {
	switch (hex) {
	case '0':
		return 'R';
	case '1':
		return 'D';
	case '2':
		return 'L';
	case '3':
		return 'U';
	}
	return 1;
}
