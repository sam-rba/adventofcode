#include <stdio.h>

#define MAXLINE 256
#define SEEDS 24
#define MAPS 48
#define FNAME "/dev/stdin"

typedef unsigned long Seed;

struct map {
	Seed dst;
	Seed src;
	int len;
};

int readseeds(Seed seeds[], int *nseeds, char line[]);
void applymaps(Seed seeds[], int nseeds, const struct map maps[], int nmaps);
Seed min(const Seed seeds[], int nseeds);
int isdigit(char c);

int
main()
{
	FILE *file;
	char line[MAXLINE];
	Seed seeds[SEEDS];
	int nseeds;
	int i;
	struct map maps[MAPS];
	int nmaps;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}

	/* read seeds */
	if (fgets(line, MAXLINE, file) == NULL || readseeds(seeds, &nseeds, line) < 0) {
		printf("input missing seed list\n");
		fclose(file);
		return 1;
	}
	printf("seeds: ");
	for (i = 0; i < nseeds; i++)
		printf("%lu ", seeds[i]);
	putchar('\n');

	nmaps = 0;
	while (fgets(line, MAXLINE, file) != NULL) {
		if (isdigit(line[0])) {
			if (nmaps >= MAPS) {
				printf("MAPS exceeded\n");
				break;
			}
			maps[nmaps].dst = maps[nmaps].src = maps[nmaps].len = 0L;
			for (i = 0; isdigit(line[i]); i++)
				maps[nmaps].dst = (maps[nmaps].dst * 10) + (line[i] - '0');
			while (isdigit(line[++i]))
				maps[nmaps].src = (maps[nmaps].src * 10) + (line[i] - '0');
			while (isdigit(line[++i]))
				maps[nmaps].len = (maps[nmaps].len * 10) + (line[i] - '0');
			nmaps++;
		} else {
			applymaps(seeds, nseeds, maps, nmaps);
			nmaps = 0;
		}
	}
	applymaps(seeds, nseeds, maps, nmaps);
	printf("Part 1: %lu\n", min(seeds, nseeds));

	fclose(file);
	return 0;
}

int
readseeds(Seed seeds[], int *nseeds, char line[])
{
	int i;

	for (i = 0; line[i] != '\0' && line[i] != ':'; i++)
		;
	if (line[i] == '\0')
		return -1;

	seeds[*nseeds = 0] = 0;
	while (line[++i] != '\0') {
		if (isdigit(line[i])) {
			seeds[*nseeds] = (seeds[*nseeds] * 10) + (line[i] - '0');
		} else if (seeds[*nseeds] != 0) {
			if (++(*nseeds) < SEEDS) {
				seeds[*nseeds] = 0;
			} else {
				printf("SEEDS exceeded\n");
				return -1;
			}
		}
	}
	return 0;
}

void
applymaps(Seed seeds[], int nseeds, const struct map maps[], int nmaps)
{
	int i, j;
	for (i = 0; i < nseeds; i++) {
		for (j = 0; j < nmaps; j++) {
			if (maps[j].src <= seeds[i] && seeds[i] < maps[j].src + maps[j].len) {
				seeds[i] = seeds[i] - maps[j].src + maps[j].dst;
				break;
			}
		}
	}
}

Seed
min(const Seed seeds[], int nseeds)
{
	Seed min;
	int i;

	min = ~0L;
	for (i = 0; i < nseeds; i++)
		min = (seeds[i] < min) ? seeds[i] : min;
	return min;
}

int
isdigit(char c)
{
	return '0' <= c && c <= '9';
}

