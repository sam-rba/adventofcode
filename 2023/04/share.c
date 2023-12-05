#include "header.h"

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

Stack *
pushnums(Stack *stk, const char *str, int start)
{
	int num;

	num = 0;
	for (; str[start] != '\0'; start++) {
		if (isdigit(str[start])) {
			num = (num * 10) + (str[start] - '0');
		} else if (num > 0) {
			stk = stpush(stk, num);
			num = 0;
		}
	}
	return stk;
}

BTree *
addnums(BTree *btr, const char *str, int *i, char delim)
{
	int num;

	num = 0;
	while (str[(*i)++] != delim) {
		if (isdigit(str[*i])) {
			num = (num * 10) + (str[*i] - '0');
		} else if (num > 0) {
			btr = btadd(btr, num);
			num = 0;
		}
	}
	return btr;
}

