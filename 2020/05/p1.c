#include <stdio.h>

#include "header.h"

int
main()
{
	char line[MAXLINE];
	int i, maxid;

	maxid = 0;
	while (lgetline(line, MAXLINE) > 0) {
		i = id(row(line), col(line));
		if (i > maxid)
			maxid = i;
	}
	printf("max id: %d\n", maxid);

	return 0;
}

