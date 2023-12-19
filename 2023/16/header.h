#define STACKMAX 128
#define NDIRECTIONS 4
#define WIDTH 128
#define HEIGHT 128

typedef enum {
	RIGHT, LEFT, UP, DOWN
} Direction;

struct Coord {
	int x;
	int y;
};

struct State {
	struct Coord pos;
	Direction dir;
};

struct Tile {
	char mirror;
	short energized;
	short visited[NDIRECTIONS];
};

struct Stack {
	struct State elems[STACKMAX];
	int len;
};

void readinput(struct Tile grid[HEIGHT][WIDTH], struct Coord *size);
int run(struct Tile grid[HEIGHT][WIDTH], struct Coord size, struct State state);
unsigned int energizedTiles(const struct Tile grid[HEIGHT][WIDTH], struct Coord size);
int push(struct Stack *stack, struct State state);
int pop(struct Stack *stack, struct State *state);

