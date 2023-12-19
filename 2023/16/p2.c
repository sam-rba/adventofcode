#include <stdio.h>

#include "header.h"

void reset(struct Tile grid[HEIGHT][WIDTH], struct Coord size);
unsigned int max(unsigned int a, unsigned int b);

int
main()
{
	static struct Tile grid[HEIGHT][WIDTH];
	struct Coord size;
	struct State state;
	int i, j;
	unsigned int maxenergized;

	readinput(grid, &size);
	maxenergized = 0;
	/* sides */
	for (i = 0; i < size.y; i++) {
		/* left edge */
		state = (struct State) { .pos={ .x=0, .y=i }, .dir=RIGHT };
		if (run(grid, size, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(grid, size), maxenergized);
		reset(grid, size);
		/* right edge */
		state = (struct State) { .pos={ .x=size.x-1, .y=i }, .dir=LEFT };
		if (run(grid, size, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(grid, size), maxenergized);
		reset(grid, size);
	}
	/* top & bottom */
	for (j = 1; j < size.x-1; j++) {
		/* top edge */
		state = (struct State) { .pos={ .x=j, .y=0 }, .dir=DOWN };
		if (run(grid, size, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(grid, size), maxenergized);
		reset(grid, size);
		/* bottom edge */
		state = (struct State) { .pos={ .x=j, .y=size.y-1 }, .dir=UP };
		if (run(grid, size, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(grid, size), maxenergized);
		reset(grid, size);
	}
	printf("part 2: %u\n", maxenergized);

	return 0;
}

void
reset(struct Tile grid[HEIGHT][WIDTH], struct Coord size)
{
	int i, j;

	for (i = 0; i < size.y; i++) {
		for (j = 0; j < size.x; j++) {
			grid[i][j].energized = 0;
			grid[i][j].visited[LEFT] = 0;
			grid[i][j].visited[RIGHT] = 0;
			grid[i][j].visited[UP] = 0;
			grid[i][j].visited[DOWN] = 0;
		}
	}
}

unsigned int
max(unsigned int a, unsigned int b) {
	return (a > b) ? a : b;
}

