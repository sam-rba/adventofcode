#include "header.h"

int
push(struct Stack *stack, struct State state)
{
	if (stack->len >= STACKMAX)
		return 1;
	stack->elems[(stack->len)++] = state;
	return 0;
}

int
pop(struct Stack *stack, struct State *state)
{
	if (stack->len <= 0)
		return 1;
	*state = stack->elems[--(stack->len)];
	return 0;
}

