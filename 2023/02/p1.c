#include <stdio.h>

#define MAXLINE 256

/* total number of cubes of each color in the bag */
#define RED 12
#define GREEN 13
#define BLUE 14

struct Set {
	int r;
	int g;
	int b;
};

int lgetline(char *line, int lim);
int parseid(char *line, int *id);
int parseset(char *line, struct Set *set);
int isdigit(char c);

int
main()
{
	int i, j, id, sum, possible;
	struct Set set;
	char line[MAXLINE];

	sum = 0;
	while (lgetline(line, MAXLINE) > 0) {
		i = parseid(line, &id);

		set.r = set.g = set.b = 0;
		possible = 1;
		for (; (j = parseset(&line[i], &set)) > 0; i += j) {
			if (set.r > RED || set.g > GREEN || set.b > BLUE) {
				possible = 0;
				break;
			}
			set.r = set.g = set.b = 0;
		}
		if (possible)
			sum += id;
	}
	printf("Part 1: %d\n", sum);
	return 0;
}

int
lgetline(char *s, int lim)
{
	int i, c;

	i = 0;
	while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (lim == 0)
		printf("Line exceeded limit, truncating\n");
	s[i] = '\0';
	return i;
}

int
parseid(char *line, int *id)
{
	int i;

	i = 0;
	while (line[i] != '\0' && !isdigit(line[i]))
		i++; /* seek to first digit */

	if (line[i] == '\0') {
		*id = -1;
		return i;
	}

	*id = 0;
	while (isdigit(line[i])) {
		*id *= 10;
		*id += line[i++]-'0';
	}

	return ++i;
}

int
parseset(char *line, struct Set *set)
{
	int i, n;

	if (line[0] == '\0')
		return -1;

	i = 0;
	while (1) {
		n = 0;

		while (isdigit(line[++i])) {
			n *= 10;
			n += line[i]-'0';
		}

		switch (line[++i]) {
		case 'r':
			set->r = n;
			break;
		case 'g':
			set->g = n;
			break;
		case 'b':
			set->b = n;
			break;
		default:
			printf("unrecognized color: %c\n", line[i]);
			return -1;
		}

		while (line[i] != '\0' && line[i] != ',' && line[i] != ';')
			i++;
		if (line[i] == '\0' || line[i++] == ';')
			break;
	}

	return i;
}


int
isdigit(char c) { return '0' <= c && c <= '9'; }

