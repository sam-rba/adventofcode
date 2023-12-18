#include <stdio.h>

#define WIDTH 128
#define HEIGHT 128
#define MAXSTACK 128

typedef enum {
	RIGHT, LEFT, UP, DOWN
} Direction;

struct tile {
	char mirror;
	short energized;
	short visited[4];
};

struct coord {
	int x;
	int y;
};

struct state {
	struct coord pos;
	Direction dir;
};

struct stack {
	struct state elems[MAXSTACK];
	int len;
};

void readinput(struct tile grid[HEIGHT][WIDTH], struct coord *size);

int
main()
{
	struct tile grid[HEIGHT][WIDTH];
	struct coord size;
	struct stack stk;
	struct coord pos;
	Direction dir;
	unsigned int energized;

	readinput(grid, &size);
	stk.elems[0].pos.x = stk.elems[0].pos.y = 0;
	stk.elems[0].dir = RIGHT;
	stk.len = 1;
	while (stk.len > 0) {
		pos = stk.elems[stk.len-1].pos;
		dir = stk.elems[stk.len-1].dir;
		if (grid[pos.y][pos.x].visited[dir]) {
			stk.len--;
			continue;
		}
		grid[pos.y][pos.x].energized = 1;
		grid[pos.y][pos.x].visited[dir] = 1;

		switch (grid[pos.y][pos.x].mirror) {
		case '.':
			switch (dir) {
			case RIGHT:
				if (pos.x < size.x-1)
					stk.elems[stk.len-1].pos.x++;
				else
					stk.len--;
				break;
			case LEFT:
				if (pos.x > 0)
					stk.elems[stk.len-1].pos.x--;
				else
					stk.len--;
				break;
			case UP:
				if (pos.y > 0)
					stk.elems[stk.len-1].pos.y--;
				else
					stk.len--;
				break;
			case DOWN:
				if (pos.y < size.y-1)
					stk.elems[stk.len-1].pos.y++;
				else
					stk.len--;
				break;
			default:
				printf("invalid direction: %d\n", dir);
				return 1;
			}
			break;
		case '|':
			switch (dir) {
			case RIGHT: /* FALLTHROUGH */
			case LEFT:
				if (pos.y > 0) {
					stk.elems[stk.len-1].pos.y--;
					stk.elems[stk.len-1].dir = UP;
				} else {
					stk.len--;
				}
				if (pos.y < size.y-1) {
					if (stk.len < MAXSTACK) {
						stk.len++;
						stk.elems[stk.len-1].pos.x = pos.x;
						stk.elems[stk.len-1].pos.y = pos.y+1;
						stk.elems[stk.len-1].dir = DOWN;
					} else {
						printf("MAXSTACK exceeded\n");
						return 1;
					}
				}
				break;
			case UP:
				if (pos.y > 0)
					stk.elems[stk.len-1].pos.y--;
				else
					stk.len--;
				break;
			case DOWN:
				if (pos.y < size.y-1)
					stk.elems[stk.len-1].pos.y++;
				else
					stk.len--;
				break;
			default:
				printf("invalid direction: %d\n", dir);
				return 1;
			}
			break;
		case '-':
			switch (dir) {
			case UP: /* FALLTHROUGH */
			case DOWN:
				if (pos.x > 0) {
					stk.elems[stk.len-1].pos.x--;
					stk.elems[stk.len-1].dir = LEFT;
				} else {
					stk.len--;
				}
				if (pos.x < size.x-1) {
					if (stk.len < MAXSTACK) {
						stk.len++;
						stk.elems[stk.len-1].pos.x = pos.x+1;
						stk.elems[stk.len-1].pos.y = pos.y;
						stk.elems[stk.len-1].dir = RIGHT;
					} else {
						printf("MAXSTACK exceeded\n");
						return 1;
					}
				}
				break;
			case LEFT:
				if (pos.x > 0)
					stk.elems[stk.len-1].pos.x--;
				else
					stk.len--;
				break;
			case RIGHT:
				if (pos.x < size.x-1)
					stk.elems[stk.len-1].pos.x++;
				else
					stk.len--;
				break;
			default:
				printf("invalid direction: %d\n", dir);
				return 1;
			}
			break;
		case '/':
			switch (dir) {
			case UP:
				if (pos.x < size.x-1) {
					stk.elems[stk.len-1].pos.x++;
					stk.elems[stk.len-1].dir = RIGHT;
				} else {
					stk.len--;
				}
				break;
			case DOWN:
				if (pos.x > 0) {
					stk.elems[stk.len-1].pos.x--;
					stk.elems[stk.len-1].dir = LEFT;
				} else {
					stk.len--;
				}
				break;
			case LEFT:
				if (pos.y < size.y-1) {
					stk.elems[stk.len-1].pos.y++;
					stk.elems[stk.len-1].dir = DOWN;
				} else {
					stk.len--;
				}
				break;
			case RIGHT:
				if (pos.y > 0) {
					stk.elems[stk.len-1].pos.y--;
					stk.elems[stk.len-1].dir = UP;
				} else {
					stk.len--;
				}
				break;
			default:
				printf("invalid direction: %d\n", dir);
				return 1;
			}
			break;
		case '\\':
			switch (dir) {
			case UP:
				if (pos.x > 0) {
					stk.elems[stk.len-1].pos.x--;
					stk.elems[stk.len-1].dir = LEFT;
				} else {
					stk.len--;
				}
				break;
			case DOWN:
				if (pos.x < size.x-1) {
					stk.elems[stk.len-1].pos.x++;
					stk.elems[stk.len-1].dir = RIGHT;
				} else {
					stk.len--;
				}
				break;
			case LEFT:
				if (pos.y > 0) {
					stk.elems[stk.len-1].pos.y--;
					stk.elems[stk.len-1].dir = UP;
				} else {
					stk.len--;
				}
				break;
			case RIGHT:
				if (pos.y < size.y-1) {
					stk.elems[stk.len-1].pos.y++;
					stk.elems[stk.len-1].dir = DOWN;
				} else {
					stk.len--;
				}
				break;
			default:
				printf("invalid direction: %d\n", dir);
				return 1;
			}
			break;
		default:
			printf("invalid tile: %c\n", grid[pos.y][pos.x].mirror);
		}
	}

	energized = 0;
	for (pos.y = 0; pos.y < size.y; pos.y++)
		for (pos.x = 0; pos.x < size.x; pos.x++)
			if (grid[pos.y][pos.x].energized)
				energized++;
	printf("part 1: %u\n", energized);

	return 0;
}

void
readinput(struct tile grid[HEIGHT][WIDTH], struct coord *size)
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

