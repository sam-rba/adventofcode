#define FNAME "/dev/stdin"
#define MAXWINNING 16
#define MAXLINE 256

#include "stack.h"

int isdigit(char c);
int cmpint(const void *a, const void *b);
Stack *pushnums(Stack *st, const char *line, int start);

