#include <stdio.h>

#include "header.h"

void reset(struct Grid *grid);
unsigned int max(unsigned int a, unsigned int b);

int
main()
{
	static struct Grid grid;
	struct State state;
	int i, j;
	unsigned int maxenergized;

	readinput(&grid);
	maxenergized = 0;
	/* sides */
	for (i = 0; i < grid.size.y; i++) {
		/* left edge */
		state = (struct State) { .pos={ .x=0, .y=i }, .dir=RIGHT };
		if (run(&grid, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(&grid), maxenergized);
		reset(&grid);
		/* right edge */
		state = (struct State) { .pos={ .x=grid.size.x-1, .y=i }, .dir=LEFT };
		if (run(&grid, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(&grid), maxenergized);
		reset(&grid);
	}
	/* top & bottom */
	for (j = 1; j < grid.size.x-1; j++) {
		/* top edge */
		state = (struct State) { .pos={ .x=j, .y=0 }, .dir=DOWN };
		if (run(&grid, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(&grid), maxenergized);
		reset(&grid);
		/* bottom edge */
		state = (struct State) { .pos={ .x=j, .y=grid.size.y-1 }, .dir=UP };
		if (run(&grid, state) != 0)
			return 1;
		maxenergized = max(energizedTiles(&grid), maxenergized);
		reset(&grid);
	}
	printf("part 2: %u\n", maxenergized);

	return 0;
}

void
reset(struct Grid *grid)
{
	int i, j;

	for (i = 0; i < grid->size.y; i++) {
		for (j = 0; j < grid->size.x; j++) {
			grid->tiles[i][j].energized = 0;
			grid->tiles[i][j].visited[LEFT] = 0;
			grid->tiles[i][j].visited[RIGHT] = 0;
			grid->tiles[i][j].visited[UP] = 0;
			grid->tiles[i][j].visited[DOWN] = 0;
		}
	}
}

unsigned int
max(unsigned int a, unsigned int b) {
	return (a > b) ? a : b;
}

