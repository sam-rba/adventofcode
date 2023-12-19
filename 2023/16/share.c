#include <stdio.h>

#include "header.h"

int move(struct Stack *stack, struct State state, char mirror, struct Coord size);

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

int
run(struct Tile grid[HEIGHT][WIDTH], struct Coord size, struct State state)
{
	struct Tile *tile;
	static struct Stack stack;

	stack.len = 0;
	if (push(&stack, state) != 0)
		return 1;
	while (pop(&stack, &state) == 0) {
		tile = &grid[state.pos.y][state.pos.x];
		if (tile->visited[state.dir]) {
			continue;
		}
		tile->energized = 1;
		tile->visited[state.dir] = 1;
		if (move(&stack, state, tile->mirror, size) != 0)
			return 1;
	}
	return 0;
}

unsigned int
energizedTiles(const struct Tile grid[HEIGHT][WIDTH], struct Coord size)
{
	unsigned int energized;
	unsigned int x, y;

	energized = 0;
	for (y = 0; y < size.y; y++)
		for (x = 0; x < size.x; x++)
			if (grid[y][x].energized)
				energized++;
	return energized;
}

int
move(struct Stack *stack, struct State state, char mirror, struct Coord size)
{
	switch (mirror) {
	case '.':
		switch (state.dir) {
		case RIGHT:
			return (++state.pos.x < size.x) ? push(stack, state) : 0;
		case LEFT:
			return (--state.pos.x >= 0) ? push(stack, state) : 0;
		case UP:
			return (--state.pos.y >= 0) ? push(stack, state) : 0;
		case DOWN:
			return (++state.pos.y < size.y) ? push(stack, state) : 0;
		default:
			printf("invalid direction: %d\n", state.dir);
			return 1;
		}
	case '|':
		switch (state.dir) {
		case RIGHT: /* FALLTHROUGH */
		case LEFT:
			if (--state.pos.y >= 0) {
				state.dir = UP;
				if (push(stack, state) != 0)
					return 1;
			}
			state.pos.y += 2;
			state.dir = DOWN;
			return (state.pos.y < size.y) ? push(stack, state) : 0;
		case UP:
			return (--state.pos.y >= 0) ? push(stack, state) : 0;
		case DOWN:
			return (++state.pos.y < size.y) ? push(stack, state) : 0;
		default:
			printf("invalid direction: %d\n", state.dir);
			return 1;
		}
	case '-':
		switch (state.dir) {
		case UP: /* FALLTHROUGH */
		case DOWN:
			if (--state.pos.x >= 0) {
				state.dir = LEFT;
				if (push(stack, state) != 0)
					return 1;
			}
			state.pos.x += 2;
			state.dir = RIGHT;
			return (state.pos.x < size.x) ? push(stack, state) : 0;
		case LEFT:
			return (--state.pos.x >= 0) ? push(stack, state) : 0;
		case RIGHT:
			return (++state.pos.x < size.x) ? push(stack, state) : 0;
		default:
			printf("invalid direction: %d\n", state.dir);
			return 1;
		}
	case '/':
		switch (state.dir) {
		case UP:
			state.dir = RIGHT;
			return (++state.pos.x < size.x) ? push(stack, state) : 0;
		case DOWN:
			state.dir = LEFT;
			return (--state.pos.x >= 0) ? push(stack, state) : 0;
		case LEFT:
			state.dir = DOWN;
			return (++state.pos.y < size.y) ? push(stack, state) : 0;
		case RIGHT:
			state.dir = UP;
			return (--state.pos.y >= 0) ? push(stack, state) : 0;
		default:
			printf("invalid direction: %d\n", state.dir);
			return 1;
		}
	case '\\':
		switch (state.dir) {
		case UP:
			state.dir = LEFT;
			return (--state.pos.x >= 0) ? push(stack, state) : 0;
		case DOWN:
			state.dir = RIGHT;
			return (++state.pos.x < size.x) ? push(stack, state) : 0;
		case LEFT:
			state.dir = UP;
			return (--state.pos.y >= 0) ? push(stack, state) : 0;
		case RIGHT:
			state.dir = DOWN;
			return (++state.pos.y < size.y) ? push(stack, state) : 0;
		default:
			printf("invalid direction: %d\n", state.dir);
			return 1;
		}
	default:
		printf("invalid tile: %c\n", mirror);
		return 1;
	}
}

