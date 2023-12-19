#include <stdio.h>

#include "header.h"

int
main()
{
	static struct Grid grid;
	struct State state;

	readinput(&grid);
	state = (struct State) { .pos={ 0, 0 }, .dir=RIGHT };
	if (run(&grid, state) != 0)
		return 1;
	printf("part 1: %u\n", energizedTiles(&grid));
	return 0;
}

