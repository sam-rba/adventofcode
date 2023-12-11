#include <stdio.h>

/* maximum grid dimensions */
#define MAXW 192
#define MAXH 192

#define MAXPOINTS 16384

typedef enum {
	NS = '|', EW = '-', NE = 'L', NW = 'J', SW = '7', SE = 'F',
	GROUND = '.', START = 'S', VISITED = '@'
} Tile;

typedef enum {
	INVALID = -1, NORTH, EAST, SOUTH, WEST
} Direction;

typedef struct coords {
	int x;
	int y;
} Coords;

int readgrid(Tile grid[MAXH][MAXW], Coords *size);
int cyclelen(const Tile grid[MAXH][MAXW], Coords size, Coords visited[MAXPOINTS], int *nvisited);
int enclosedtiles(const Tile grid[MAXH][MAXW], Coords size, const Coords visited[], int nvisited);
int startpos(Coords *pos, const Tile grid[MAXH][MAXW], Coords size);
Direction choosedir(const Tile grid[MAXH][MAXW], Coords size, Coords *pos);
Direction move(const Tile grid[MAXH][MAXW], Coords size, Coords *pos, Direction dir);
int accessiblefrom(Tile tile, Direction dir);
Direction outdir(Tile tile, Direction indir);
int abs(int a);
void printdir(Direction dir);
void printgrid(const Tile grid[MAXH][MAXW], Coords size);

int
main()
{
	Tile grid[MAXH][MAXW];
	Coords size; /* dimensions of grid */
	Coords visited[MAXPOINTS];
	int nvisited;
	int len;

	if (readgrid(grid, &size) != 0) {
		return 1;
	}

	printgrid(grid, size);
	printf("cycle len: %d\n", len = cyclelen(grid, size, visited, &nvisited));
	printf("1/2 cycle len (part 1): %d\n", len/2);
	printf("enclosed tiles (part 2): %d\n", enclosedtiles(grid, size, visited, nvisited));

	return 0;
}

int
readgrid(Tile grid[MAXH][MAXW], Coords *size)
{
	int i, c;

	size->y = size->x = 0;
	for (i = 0; (c = getchar()) != EOF; i++) {
		if (c == '\n') {
			size->y++;
			if (size->y > MAXH) {
				printf("MAXH exceeded\n");
				return 1;
			}
			size->x = i;
			i = -1;
		} else if (i >= MAXW) {
			printf("MAXW exceeded\n");
			return 1;
		} else {
			grid[size->y][i] = c;
		}
	}
	return 0;
}

/* length of a cycle starting from the starting tile */
int
cyclelen(const Tile grid[MAXH][MAXW], Coords size, Coords visited[MAXPOINTS], int *nvisited)
{
	Coords pos;
	Direction dir;
	int len;

	if (startpos(&pos, grid, size) != 0) {
		printf("start position not found\n");
		return -1;
	}
	printf("start: (y=%d, x=%d)\n", pos.y, pos.x);

	*nvisited = 0;
	if (*nvisited < MAXPOINTS) {
		visited[(*nvisited)++] = pos;
	} else {
		printf("MAXPOINTS exceeded\n");
		return -1;
	}

	dir = choosedir(grid, size, &pos);

	for (len = 1; grid[pos.y][pos.x] != START; len++) {
		if (*nvisited < MAXPOINTS) {
			visited[(*nvisited)++] = pos;
		} else {
			printf("MAXPOINTS exceeded\n");
			return -1;
		}
		if ((dir = move(grid, size, &pos, dir)) < 0) {
			return -1;
		}
	}
	return len;
}

int
enclosedtiles(const Tile grid[MAXH][MAXW], Coords size,
		const Coords visited[MAXPOINTS], int nvisited)
{
	int i;
	int area;
	int interiorpoints;

	/* shoelace formula */
	area = 0;
	for (i = 0; i < nvisited-1; i++) {
		area += (visited[i].x * visited[i+1].y) - (visited[i+1].x * visited[i].y);
	}
	area += (visited[nvisited-1].x * visited[0].y) - (visited[0].x * visited[nvisited-1].y);
	area = abs(area) / 2;
	printf("area: %d\n", area);

	/* Pick's theorem */
	interiorpoints = area - (nvisited/2) + 1;
	return interiorpoints;
}

int
startpos(Coords *pos, const Tile grid[MAXH][MAXW], Coords size)
{
	int x, y;
	for (y = 0; y < size.y; y++) {
		for (x = 0; x < size.x; x++) {
			if (grid[y][x] == START) {
				pos->y = y;
				pos->x = x;
				return 0;
			}
		}

	}
	return 1;
}

/* Arbitrarily choose one of two directions out of current tile.
   *pos is moved one step in that direction. */
Direction
choosedir(const Tile grid[MAXH][MAXW], Coords size, Coords *pos)
{
	if (pos->y > 0 && accessiblefrom(grid[pos->y-1][pos->x], SOUTH)) {
		pos->y--;
		return NORTH;
	} else if (pos->x < size.x && accessiblefrom(grid[pos->y][pos->x+1], WEST)) {
		pos->x++;
		return EAST;
	} else if (pos->y < size.y && accessiblefrom(grid[pos->y+1][pos->x], NORTH)) {
		pos->y++;
		return SOUTH;
	} else if (pos->x > 0 && accessiblefrom(grid[pos->y][pos->x-1], EAST)) {
		pos->x--;
		return WEST;
	}
	printf("unreachable\n");
	return -1;
}

Direction
move(const Tile grid[MAXH][MAXW], Coords size, Coords *pos, Direction dir)
{
	switch (outdir(grid[pos->y][pos->x], dir)) {
	case NORTH:
		if (pos->y <= 0) {
			printf("dead end going north at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		pos->y--;
		return NORTH;
	case EAST:
		if (pos->x >= size.x) {
			printf("dead end going east at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		pos->x++;
		return EAST;
	case SOUTH:
		if (pos->y >= size.y) {
			printf("dead end going south at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		pos->y++;
		return SOUTH;
	case WEST:
		if (pos->x <= 0) {
			printf("dead end going west at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		pos->x--;
		return WEST;
	default:
		printf("invalid direction\n");
		return -1;
	}
}

int
accessiblefrom(Tile tile, Direction dir)
{
	switch (dir) {
	case NORTH:
		switch (tile) {
		case NS: /* FALLTHROUGH */
		case NE:
		case NW:
			return 1;
		}
		break;
	case EAST:
		switch (tile) {
		case EW: /* FALLTHROUGH */
		case NE:
		case SE:
			return 1;
		}
		break;
	case SOUTH:
		switch (tile) {
		case NS: /* FALLTHROUGH */
		case SW:
		case SE:
			return 1;
		}
		break;
	case WEST:
		switch (tile) {
		case EW: /* FALLTHROUGH */
		case NW:
		case SW:
			return 1;
		}
		break;
	}
	return 0;
}

/* indir is the direction we were traveling towards when tile was entered.
   i.e. if indir is NORTH, then tile was entered from the SOUTH. */
Direction
outdir(Tile tile, Direction indir)
{
	switch (indir) {
	case NORTH:
		switch (tile) {
		case NS:
			return NORTH;
		case SW:
			return WEST;
		case SE:
			return EAST;
		}
		break;
	case EAST:
		switch (tile) {
		case EW:
			return EAST;
		case NW:
			return NORTH;
		case SW:
			return SOUTH;
		}
		break;
	case SOUTH:
		switch (tile) {
		case NS:
			return SOUTH;
		case NE:
			return EAST;
		case NW:
			return WEST;
		}
		break;
	case WEST:
		switch (tile) {
		case EW:
			return WEST;
		case NE:
			return NORTH;
		case SE:
			return SOUTH;
		}
		break;
	default:
		printf("invalid direction: %d\n", indir);
	}
	printf("%c is not accessible from the ", tile);
	printdir(indir);
	return -1;
}

int
abs(int a)
{
	return (a < 0) ? -a : a;
}

void
printdir(Direction dir)
{
	switch (dir) {
	case NORTH:
		printf("north\n");
		break;
	case EAST:
		printf("east\n");
		break;
	case SOUTH:
		printf("south\n");
		break;
	case WEST:
		printf("west\n");
		break;
	default:
		printf("invalid direction: %d\n", dir);
	}
}

void
printgrid(const Tile grid[MAXH][MAXW], Coords size)
{
	int y, x;

	for (y = 0; y < size.y; y++) {
		for (x = 0; x < size.x; x++) {
			putchar(grid[y][x]);
		}
		putchar('\n');
	}
}

