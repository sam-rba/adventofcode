#include <stdio.h>

#include "header.h"

void fill(int *arr, int size);
void intersection(int *a, int *b, int size);

int main()
{
	int all[QUESTIONS];
	int cur[QUESTIONS];
	char line[MAXLINE];
	int i, sum;

	fill(all, QUESTIONS);
	clear(cur, QUESTIONS);
	sum = 0;
	while (lgetline(line, MAXLINE) > 0) {
		if (line[0]  == '\n') {
			sum += counttrue(all, QUESTIONS);
			fill(all, QUESTIONS);
		} else {
			i = 0;
			while (line[i] != '\0')
				cur[line[i++]-'a'] = 1;
			intersection(all, cur, QUESTIONS);
		}
		clear(cur, QUESTIONS);
	}
	sum += counttrue(all, QUESTIONS);
	printf("Part 2: %d\n", sum);
	return 0;
}

void
fill(int *arr, int size)
{
	int i;
	for (i = 0; i < size; i++)
		arr[i] = 1;
}

void
intersection(int *a, int *b, int size)
{
	int i;
	for (i = 0; i < size; i++)
		a[i] = (a[i] && b[i]);
}

