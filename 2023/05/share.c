#include "header.h"

void
readmap(struct map *mapp, const char line[])
{
	int i;

	mapp->dst = mapp->src = mapp->len = 0L;
	for (i = 0; isdigit(line[i]); i++)
		mapp->dst = (mapp->dst * 10) + (line[i] - '0');
	while (isdigit(line[++i]))
		mapp->src = (mapp->src * 10) + (line[i] - '0');
	while (isdigit(line[++i]))
		mapp->len = (mapp->len * 10) + (line[i] - '0');
}

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

