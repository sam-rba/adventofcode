#include <stdio.h>

#include "header.h"

#define WIDTH 128
#define HEIGHT 128
#define NDIRECTIONS 4

struct Tile {
	char mirror;
	short energized;
	short visited[NDIRECTIONS];
};

void readinput(struct Tile grid[HEIGHT][WIDTH], struct Coord *size);

int
main()
{
	struct Tile grid[HEIGHT][WIDTH];
	struct Coord size;

	struct Stack stack;
	struct State state;
	struct Tile *tile;

	unsigned int energized;

	readinput(grid, &size);
	state.pos.x = state.pos.y = 0;
	state.dir = RIGHT;
	if (push(&stack, state) != 0)
		return 1;
	while (pop(&stack, &state) == 0) {
		tile = &grid[state.pos.y][state.pos.x];
		if (tile->visited[state.dir]) {
			continue;
		}
		tile->energized = 1;
		tile->visited[state.dir] = 1;

		switch (tile->mirror) {
		case '.':
			switch (state.dir) {
			case RIGHT:
				if (++state.pos.x < size.x && push(&stack, state) != 0)
					return 1;
				break;
			case LEFT:
				if (--state.pos.x >= 0 && push(&stack, state) != 0)
					return 1;
				break;
			case UP:
				if (--state.pos.y >= 0 && push(&stack, state) != 0)
					return 1;
				break;
			case DOWN:
				if (++state.pos.y < size.y && push(&stack, state) != 0)
					return 1;
				break;
			default:
				printf("invalid direction: %d\n", state.dir);
				return 1;
			}
			break;
		case '|':
			switch (state.dir) {
			case RIGHT: /* FALLTHROUGH */
			case LEFT:
				if (--state.pos.y >= 0) {
					state.dir = UP;
					if (push(&stack, state) != 0)
						return 1;
				}
				state.pos.y++;
				if (++state.pos.y < size.y) {
					state.dir = DOWN;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case UP:
				if (--state.pos.y >= 0 && push(&stack, state) != 0)
					return 1;
				break;
			case DOWN:
				if (++state.pos.y < size.y && push(&stack, state) != 0)
					return 1;
				break;
			default:
				printf("invalid direction: %d\n", state.dir);
				return 1;
			}
			break;
		case '-':
			switch (state.dir) {
			case UP: /* FALLTHROUGH */
			case DOWN:
				if (--state.pos.x >= 0) {
					state.dir = LEFT;
					if (push(&stack, state) != 0)
						return 1;
				}
				state.pos.x++;
				if (++state.pos.x < size.x) {
					state.dir = RIGHT;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case LEFT:
				if (--state.pos.x >= 0 && push(&stack, state) != 0)
					return 1;
				break;
			case RIGHT:
				if (++state.pos.x < size.x && push(&stack, state) != 0)
					return 1;
				break;
			default:
				printf("invalid direction: %d\n", state.dir);
				return 1;
			}
			break;
		case '/':
			switch (state.dir) {
			case UP:
				if (++state.pos.x < size.x) {
					state.dir = RIGHT;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case DOWN:
				if (--state.pos.x >= 0) {
					state.dir = LEFT;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case LEFT:
				if (++state.pos.y < size.y) {
					state.dir = DOWN;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case RIGHT:
				if (--state.pos.y >= 0) {
					state.dir = UP;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			default:
				printf("invalid direction: %d\n", state.dir);
				return 1;
			}
			break;
		case '\\':
			switch (state.dir) {
			case UP:
				if (--state.pos.x >= 0) {
					state.dir = LEFT;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case DOWN:
				if (++state.pos.x < size.x) {
					state.dir = RIGHT;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case LEFT:
				if (--state.pos.y >= 0) {
					state.dir = UP;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			case RIGHT:
				if (++state.pos.y < size.y) {
					state.dir = DOWN;
					if (push(&stack, state) != 0)
						return 1;
				}
				break;
			default:
				printf("invalid direction: %d\n", state.dir);
				return 1;
			}
			break;
		default:
			printf("invalid tile: %c\n", grid[state.pos.y][state.pos.x].mirror);
			return 1;
		}
	}

	energized = 0;
	for (state.pos.y = 0; state.pos.y < size.y; state.pos.y++)
		for (state.pos.x = 0; state.pos.x < size.x; state.pos.x++)
			if (grid[state.pos.y][state.pos.x].energized)
				energized++;
	printf("part 1: %u\n", energized);

	return 0;
}

void
readinput(struct Tile grid[HEIGHT][WIDTH], struct Coord *size)
{
	int j, c;

	size->x = size->y = j = 0;
	while ((c = getchar()) != EOF) {
		if (c == '\n') {
			if (size->y >= HEIGHT) {
				printf("HEIGHT exceeded\n");
				return;
			}
			size->y++;
			size->x = j;
			j = 0;
		} else if (j < WIDTH) {
			grid[size->y][j].mirror = c;
			grid[size->y][j].energized = 0;
			grid[size->y][j].visited[RIGHT] = 0;
			grid[size->y][j].visited[LEFT] = 0;
			grid[size->y][j].visited[UP] = 0;
			grid[size->y][j++].visited[DOWN] = 0;
		} else {
			printf("WIDTH exceeded\n");
			return;
		}
	}
}

