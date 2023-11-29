#include <stdio.h>

#include "header.h"

#define MAXID 1024

void sort(int *arr, int size);
void swap(int *arr, int x, int y);

int
main()
{
	char line[MAXLINE];
	int i;
	int ids[MAXID];

	i = 0;
	while (lgetline(line, MAXLINE) > 0)
		ids[i++] = id(row(line), col(line));

	sort(ids, i);

	while (--i > 0) {
		if (ids[i] != ids[i-1]+1) {
			printf("%d\n", ids[i]-1);
			return 0;
		}
	}
	printf("not found\n");
	return 1;
}

/* Insertion sort */
void
sort(int *arr, int size)
{
	int i, j;
	for (i = 1; i < size; i++)
		for (j = i; j > 0 && arr[j-1] > arr[j]; j--)
			swap(arr, j, j-1);
}

/* XOR swap */
void
swap(int *arr, int x, int y)
{
	arr[x] = arr[x]^arr[y];
	arr[y] = arr[y]^arr[x];
	arr[x] = arr[x]^arr[y];
}

