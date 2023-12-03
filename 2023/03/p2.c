#include <stdio.h>

#include "header.h"

int ratio(char *line0, char *line1, char *line2);

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
			if (isdigit(line0[geari])) {
				for (i = geari-1; i >= 0 && isdigit(line0[i]); i--)
					;
				while (isdigit(line0[++i]))
					*partp = (*partp * 10) + (line0[i] - '0');
				partp = &part2;
			} else {
				/* top left */
				if (geari > 0 && isdigit(line0[geari-1])) {
					for (i = geari-2; i >= 0 && isdigit(line0[i]); i--)
						;
					while (isdigit(line0[++i]))
						*partp = (*partp * 10) + (line0[i] - '0');
					partp = &part2;
				}
				/* top right */
				if (isdigit(line0[geari+1])) {
					for (i = geari+1; isdigit(line0[i]); i++)
						*partp = (*partp * 10) + (line0[i] - '0');
					partp = (partp == &part1) ? &part2 : NULL;
				}
			}

			/* left */
			if (geari > 0 && isdigit(line1[geari-1])) {
				if (partp == NULL) /* more than 2 adj. parts */
					continue;
				for (i = geari-2; i >= 0 && isdigit(line1[i]); i--)
					;
				while (isdigit(line1[++i]))
					*partp = (*partp * 10) + (line1[i] - '0');
				partp = (partp == &part1) ? &part2 : NULL;
			}

			/* right */
			if (isdigit(line1[geari+1])) {
				if (partp == NULL) /* more than 2 adj. parts */
					continue;
				for (i = geari+1; isdigit(line1[i]); i++)
					*partp = (*partp * 10) + (line1[i] - '0');
				partp = (partp == &part1) ? &part2 : NULL;
			}

			/* bottom middle */
			if (isdigit(line2[geari])) {
				if (partp == NULL) /* more than 2 adj. parts */
					continue;
				for (i = geari-1; i >= 0 && isdigit(line2[i]); i--)
					;
				while (isdigit(line2[++i]))
					*partp = (*partp * 10) + (line2[i] - '0');
				partp = (partp == &part1) ? &part2 : NULL;
			} else {
				/* bottom left */
				if (geari > 0 && isdigit(line2[geari-1])) {
					if (partp == NULL) /* more than 2 adj. parts */
						continue;
					for (i = geari-2; i >= 0 && isdigit(line2[i]); i--)
						;
					while (isdigit(line2[++i]))
						*partp = (*partp * 10) + (line2[i] - '0');
					partp = (partp == &part1) ? &part2 : NULL;
				}
				/* bottom right */
				if (isdigit(line2[geari+1])) {
					if (partp == NULL) /* more than 2 adj. parts */
						continue;
					for (i = geari+1; isdigit(line2[i]); i++)
						*partp = (*partp * 10) + (line2[i] - '0');
				}
			}
			sum += part1 * part2;
		}
	}
	return sum;
}

