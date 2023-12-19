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

struct Grid {
	struct Tile tiles[HEIGHT][WIDTH];
	struct Coord size;
};

struct Stack {
	struct State elems[STACKMAX];
	int len;
};

void readinput(struct Grid *grid);
int run(struct Grid *grid, struct State state);
unsigned int energizedTiles(const struct Grid *grid);
int push(struct Stack *stack, struct State state);
int pop(struct Stack *stack, struct State *state);

