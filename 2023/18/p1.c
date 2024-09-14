#include "header.h"

int
main() {
	Dig *digs;
	Point *points;
	
	digs = parse(stdin);
	points = corners(digs);
	digfree(digs);
	printf("silver: %llu\n", area(points));
	pointfree(points);
}

int
parseline(const char *line, char *dir, unsigned int *dist) {
	if (sscanf(line, "%c %u", dir, dist) != 2) {
		fprintf(stderr, "invalid input: %s", line);
		return 1;
	}
	return 0;
}
