#define STACKMAX 128

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

struct Stack {
	struct State elems[STACKMAX];
	int len;
};

int push(struct Stack *stack, struct State state);
int pop(struct Stack *stack, struct State *state);

