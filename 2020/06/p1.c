#include <stdio.h>

#include "header.h"

int
main()
{
	int i, sum;
	char line[MAXLINE];
	int questions[QUESTIONS];

	clear(questions, QUESTIONS);
	sum = 0;
	while (lgetline(line, MAXLINE) > 0) {
		if (line[0] == '\n') {
			sum += counttrue(questions, QUESTIONS);
			clear(questions, QUESTIONS);
			continue;
		}
		i = 0;
		while (line[i] != '\0')
			questions[line[i++]-'a'] = 1;
	}
	sum += counttrue(questions, QUESTIONS);
	printf("Part 1: %d\n", sum);
	return 0;
}

