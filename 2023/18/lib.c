#include <stdlib.h>
#include <string.h>

#include "header.h"

enum { MAXLINE = 32 };

static unsigned long long shoelace(Point *points);
static unsigned long long perimeter(Point *points);
static unsigned int dist(const Point *p1, const Point *p2);

void
digfree(Dig *d) {
	if (d == NULL)
		return;
	digfree(d->next);
	free(d);
}

int
push(Point **p, int x, int y) {
	Point *new;
	
	if ((new = malloc(sizeof(Point))) == NULL) {
		fprintf(stderr, "failed to allocate Point\n");
		return 1;
	}
	new->x = x;
	new->y = y;
	new->next = *p;
	*p = new;
	return 0;
}

void
pointfree(Point *p) {
	if (p == NULL)
		return;
	pointfree(p->next);
	free(p);
}

Dig *
parse(FILE *stream) {
	static char line[MAXLINE];
	Dig *head, *curr;
	char dir;
	unsigned int dist;

	head = curr = NULL;
	while (fgets(line, MAXLINE, stream) != NULL) {
		if (strlen(line) >= MAXLINE-1)
			fprintf(stderr, "max line length (%d) exceeded\n", MAXLINE);

		if (parseline(line, &dir, &dist))
			break;

		if (head == NULL) {
			head = malloc(sizeof(Dig));
			curr = head;
		} else {
			curr->next = malloc(sizeof(Dig));
			curr = curr->next;
		}
		if (curr == NULL) {
			fprintf(stderr, "failed to allocate Dig\n");
			break;
		}

		curr->dir = dir;
		curr->dist = dist;
		curr->next = NULL;
	}

	return head;
}

Point *
corners(Dig *digs) {
	Point *points, p;
	Dig *digp;

	p.x = 0; p.y = 0; p.next = NULL;
	points = NULL;
	for (digp = digs; digp != NULL; digp = digp->next) {
		switch (digp->dir) {
		case 'U':
			p.y -= digp->dist;
			break;
		case 'D':
			p.y += digp->dist;
			break;
		case 'L':
			p.x -= digp->dist;
			break;
		case 'R':
			p.x += digp->dist;
			break;
		default:
			fprintf(stderr, "invalid direction: %c\n", digp->dir);
			return points;
		}

		if (push(&points, p.x, p.y))
			break;
	}

	return points;
}

unsigned long long
area(Point *points) {
	return shoelace(points) + perimeter(points)/2 + 1;
}

static unsigned long long
shoelace(Point *points) {
	Point *p;
	long long a, x1, x2, y1, y2;

	if (points == NULL
			|| points->next == NULL
			|| points->next->next == NULL)
		return 0;

	a = 0;
	for (p = points; p->next != NULL; p = p->next) {
		x1 = p->x; x2 = p->next->x;
		y1 = p->y; y2 = p->next->y;
		a += x1*y2 - x2*y1;
	}

	x1 = p->x; y1 = p->y;
	x2 = points->x; y2 = points->y;
	a += x1*y2 - x2*y1;

	a = (a < 0) ? -a : a;
	return a/2;
}

static unsigned long long
perimeter(Point *points) {
	unsigned long long perim;
	Point *p;

	if (points == NULL)
		return 0;
	
	perim = 0;
	for (p = points; p->next != NULL; p = p->next) {
		perim += dist(p, p->next);
	}
	perim += dist(p, points);
	return perim;
}

static unsigned int
dist(const Point *p1,  const Point *p2) {
	return abs(p1->x - p2->x) + abs(p1->y - p2->y);
}
