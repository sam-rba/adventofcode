#include <stdio.h>

int
lgetline(char *line, int size)
{
	int i, c;

	i = 0;
	while (i < size && (c = getchar()) != EOF && c != '\n')
		line[i++] = c;
	if (i >= size) {
		printf("Line length exceeded %d, truncating.\n", size);
		line[size-1] = '\0';
		return size-1;
	}
	if (c == '\n' && i < size-1)
		line[i++] = c;
	line[i] = '\0';
	return i;
}

int
counttrue(int *arr, int size)
{
	int i, count;
	for (i = count = 0; i < size; i++)
		if (arr[i])
			count++;
	return count;
}

void
clear(int *arr, int size)
{
	int i;
	for (i = 0; i < size; i++)
		arr[i] = 0;
}

