#include <stdio.h>

/* maximum grid dimensions */
#define MAXW 192
#define MAXH 192

typedef enum {
	NS = '|', EW = '-', NE = 'L', NW = 'J', SW = '7', SE = 'F',
	GROUND = '.', START = 'S'
} Tile;

typedef enum {
	NORTH, EAST, SOUTH, WEST
} Direction;

struct coords {
	int x;
	int y;
};

int readgrid(Tile grid[MAXH][MAXW], struct coords *size);
int cyclelen(const Tile grid[MAXH][MAXW], struct coords size);
int startpos(struct coords *pos, const Tile grid[MAXH][MAXW], struct coords size);
Direction choosedir(const Tile grid[MAXH][MAXW], struct coords size, struct coords *pos);
Direction move(const Tile grid[MAXH][MAXW], struct coords size, struct coords *pos, Direction dir);
int accessiblefrom(Tile tile, Direction dir);
Direction outdir(Tile tile, Direction indir);
void printgrid(const Tile grid[MAXH][MAXW], struct coords size);
void printdir(Direction dir);

int
main()
{
	Tile grid[MAXH][MAXW];
	struct coords size; /* dimensions of grid */
	int len;

	if (readgrid(grid, &size) != 0) {
		return 1;
	}

	printgrid(grid, size);
	printf("loop length: %d\n", len = cyclelen(grid, size));
	printf("part 1: %d\n", len/2);

	return 0;
}

int
readgrid(Tile grid[MAXH][MAXW], struct coords *size)
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
cyclelen(const Tile grid[MAXH][MAXW], struct coords size)
{
	struct coords pos;
	Direction dir;
	int len;

	if (startpos(&pos, grid, size) != 0) {
		printf("start position not found\n");
		return -1;
	}
	printf("start: (%d, %d) (y, x)\n", pos.y, pos.x);

	dir = choosedir(grid, size, &pos);

	for (len = 1; grid[pos.y][pos.x] != START; len++) {
		if ((dir = move(grid, size, &pos, dir)) < 0) {
			return -1;
		}
	}
	return len;
}

int
startpos(struct coords *pos, const Tile grid[MAXH][MAXW], struct coords size)
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
choosedir(const Tile grid[MAXH][MAXW], struct coords size, struct coords *pos)
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
move(const Tile grid[MAXH][MAXW], struct coords size, struct coords *pos, Direction dir)
{
	switch (outdir(grid[pos->y][pos->x], dir)) {
	case NORTH:
		if (pos->y <= 0) {
			printf("dead end going north at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		printf("(y=%d, x=%d), go north\n", pos->y, pos->x);
		pos->y--;
		return NORTH;
	case EAST:
		if (pos->x >= size.x) {
			printf("dead end going east at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		printf("(y=%d, x=%d), go east\n", pos->y, pos->x);
		pos->x++;
		return EAST;
	case SOUTH:
		if (pos->y >= size.y) {
			printf("dead end going south at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		printf("(y=%d, x=%d), go south\n", pos->y, pos->x);
		pos->y++;
		return SOUTH;
	case WEST:
		if (pos->x <= 0) {
			printf("dead end going west at (y=%d, x=%d)\n", pos->y, pos->x);
			return -1;
		}
		printf("(y=%d, x=%d), go west\n", pos->y, pos->x);
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

void
printgrid(const Tile grid[MAXH][MAXW], struct coords size)
{
	int i, j;

	printf("width: %d, height: %d\n\n", size.x, size.y);
	for (i = 0; i < size.y; i++) {
		for (j = 0; j < size.x; j++) {
			putchar(grid[i][j]);
			putchar(' ');
		}
		putchar('\n');
	}
	putchar('\n');
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

