#include <stdio.h>
#include <string.h>

#define MAXLINE 256

void zero(char *line);
int partsinwindow(char *line0, char *line1, char *line2);
void shiftwindow(char *line0, char *line1, char *line2);
int adjacent(char *line0, char *line1, char *line2, int pos);
int issymbol(char c);
int isdigit(char c);

int
main()
{
	int sum, eof;
	char lines[3][MAXLINE];
	FILE *file;

	file = fopen("/dev/stdin", "r");
	zero(lines[0]);
	sum = 0;
	eof = (fgets(lines[1], MAXLINE, file) == NULL);
	while (!eof) {
		if (fgets(lines[2], MAXLINE, file) == NULL) {
			zero(lines[2]);
			eof = 1;
		}
		sum += partsinwindow(lines[0], lines[1], lines[2]);
		shiftwindow(lines[0], lines[1], lines[2]);
	}

	printf("Part 1: %d\n", sum);
	fclose(file);
	return 0;
}

void
zero(char *line)
{
	int i;
	for (i = 0; i < MAXLINE-1; i++)
		line[i] = '.';
	line[MAXLINE-1] = '\0';
}

int
partsinwindow(char *line0, char *line1, char *line2)
{
	int i, sum, partnum, adj;

	sum = partnum = adj = 0;
	for (i = 0; 1; i++) {
		if (isdigit(line1[i])) {
			partnum = (partnum * 10) + (line1[i] - '0');
			adj = (adj) ? adj : adjacent(line0, line1, line2, i);
		} else {
			if (adj)
				sum += partnum;
			partnum = adj = 0;
		}
		if (line1[i] == '\n')
			break;
	}
	return sum;
}

void
shiftwindow(char *line0, char *line1, char *line2)
{
	strcpy(line0, line1);
	strcpy(line1, line2);
}

int
adjacent(char *line0, char *line1, char *line2, int pos)
{
	/* immediately above or below */
	if (issymbol(line0[pos]) || issymbol(line2[pos]))
		return 1;
	/* left (including diagonal) */
	if (pos > 0 && (issymbol(line0[pos-1])
				|| issymbol(line1[pos-1])
				|| issymbol(line2[pos-1])))
		return 1;
	/* right (including diagonal) */
	if (line1[pos+1] != '\n' && (issymbol(line0[pos+1])
				|| issymbol(line1[pos+1])
				|| issymbol(line2[pos+1])))
		return 1;
	return 0;
}

int
issymbol(char c)
{
	return c != '.' && !isdigit(c);
}

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

