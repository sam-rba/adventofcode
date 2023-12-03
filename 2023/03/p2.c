#include <stdio.h>

#include "header.h"

int ratio(char *line0, char *line1, char *line2);
int partat(char *line, int pos, int *part);
int parttoleft(char *line, int pos, int *part);
int parttoright(char *line, int pos, int *part);

int
main()
{
	int eof, sum, part1, part2;
	char lines[3][MAXLINE];
	FILE *file;

	file = fopen("/dev/stdin", "r");
	sum = 0;
	zero(lines[0]);
	eof = (fgets(lines[1], MAXLINE, file) == NULL);
	while (!eof) {
		if (fgets(lines[2], MAXLINE, file) == NULL) {
			zero(lines[2]);
			eof = 1;
		}
		sum += ratio(lines[0], lines[1], lines[2]);
		shiftwindow(lines[0], lines[1], lines[2]);
	}

	printf("Part 2: %d\n", sum);
	fclose(file);
	return 0;
}

int
ratio(char *line0, char *line1, char *line2)
{
	int sum, geari, part1, part2, i;
	int *partp;

	sum = 0;
	for (geari = 0; line1[geari] != '\n'; geari++) {
		part1 = part2 = 0;
		partp = &part1;
		if (line1[geari] == '*') { /* look for adjacent part nums */
			/* top middle */
			if (partat(line0, geari, partp)) {
				partp = &part2;
			} else {
				/* top left */
				if (parttoleft(line0, geari, partp))
					partp = &part2;

				/* top right */
				if (parttoright(line0, geari, partp))
					partp = (partp == &part1) ? &part2 : NULL;
			}
			/* left */
			if (parttoleft(line1, geari, partp)) {
				if (partp == NULL) /* more than 2 adj. parts */
					continue;
				partp = (partp == &part1) ? &part2 : NULL;
			}
			/* right */
			if (parttoright(line1, geari, partp)) {
				if (partp == NULL) /* more than 2 adj. parts */
					continue;
				partp = (partp == &part1) ? &part2 : NULL;
			}
			/* bottom middle */
			if (partat(line2, geari, partp)) {
				if (partp == NULL) /* more than 2 adj. parts */
					continue;
				partp = (partp == &part1) ? &part2 : NULL;
			} else {
				/* bottom left */
				if (parttoleft(line2, geari, partp)) {
					if (partp == NULL) /* more than 2 adj. parts */
						continue;
					partp = (partp == &part1) ? &part2 : NULL;
				}
				/* bottom right */
				if (parttoright(line2, geari, partp)) {
					if (partp == NULL) /* more than 2 adj. parts */
						continue;
					partp = (partp == &part1) ? &part2 : NULL;
				}
			}
			sum += part1 * part2;
		}
	}
	return sum;
}

int
partat(char *line, int pos, int *part)
{
	int i;

	if (isdigit(line[pos])) {
		if (part != NULL) {
			for (i = pos-1; i >= 0 && isdigit(line[i]); i--)
				;
			while (isdigit(line[++i]))
				*part = (*part * 10) + (line[i] - '0');
		}
		return 1;
	}
	return 0;
}

int
parttoleft(char *line, int pos, int *part)
{
	int i;

	if (pos > 0 && isdigit(line[pos-1])) {
		if (part != NULL) {
			for (i = pos-2; i >= 0 && isdigit(line[i]); i--)
				;
			while (isdigit(line[++i]))
				*part = (*part * 10) + (line[i] - '0');
		}
		return 1;
	}
	return 0;
}

int
parttoright(char *line, int pos, int *part)
{
	int i;
	if (isdigit(line[pos+1])) {
		if (part != NULL)
			for (i = pos+1; isdigit(line[i]); i++)
				*part = (*part * 10) + (line[i] - '0');
		return 1;
	}
	return 0;
}

