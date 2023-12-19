#include <stdio.h>

#include "header.h"

int
main()
{
	static struct Tile grid[HEIGHT][WIDTH];
	struct Coord size;
	struct State state;

	readinput(grid, &size);
	state = (struct State) { .pos={ 0, 0 }, .dir=RIGHT };
	if (run(grid, size, state) != 0)
		return 1;
	printf("part 1: %u\n", energizedTiles(grid, size));
	return 0;
}

