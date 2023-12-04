#include "header.h"

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

Stack *
pushnums(Stack *st, const char *line, int start)
{
	int num;

	num = 0;
	for (; line[start] != '\0'; start++) {
		if (isdigit(line[start])) {
			num = (num * 10) + (line[start] - '0');
		} else if (num > 0) {
			st = stpush(st, num);
			num = 0;
		}
	}
	return st;
}

