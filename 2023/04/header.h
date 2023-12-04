#define FNAME "/dev/stdin"
#define MAXLINE 256

#include "stack.h"

int isdigit(char c);
Stack *pushnums(Stack *st, const char *line, int start);

