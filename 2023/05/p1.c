#include <stdio.h>

#include "header.h"

#define SEEDS 24

typedef unsigned long Seed;

int readseeds(Seed seeds[], const char line[]);
void applymaps(Seed seeds[], int nseeds, const struct map maps[], int nmaps);
Seed min(const Seed seeds[], int nseeds);

int
main()
{
	FILE *file;
	char line[MAXLINE];
	Seed seeds[SEEDS];
	int nseeds;
	int i;
	struct map maps[MAPS], *mapp;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}

	if (fgets(line, MAXLINE, file) == NULL
			|| (nseeds = readseeds(seeds, line)) < 1) {
		printf("input missing seed list\n");
		fclose(file);
		return 1;
	}

	mapp = maps;
	while (fgets(line, MAXLINE, file) != NULL) {
		if (isdigit(line[0])) {
			if (mapp >= maps+MAPS) {
				printf("MAPS exceeded\n");
				break;
			}
			readmap(mapp++, line);
		} else {
			applymaps(seeds, nseeds, maps, mapp-maps);
			mapp = maps;
		}
	}
	applymaps(seeds, nseeds, maps, mapp-maps);
	printf("Part 1: %lu\n", min(seeds, nseeds));

	fclose(file);
	return 0;
}

int
readseeds(Seed seeds[], const char line[])
{
	int i, nseeds;

	for (i = 0; line[i] != '\0' && line[i] != ':'; i++)
		;
	if (line[i] == '\0')
		return -1;

	seeds[nseeds = 0] = 0;
	while (line[++i] != '\0') {
		if (isdigit(line[i])) {
			seeds[nseeds] = (seeds[nseeds] * 10) + (line[i] - '0');
		} else if (seeds[nseeds] != 0) {
			if (++nseeds < SEEDS) {
				seeds[nseeds] = 0;
			} else {
				printf("SEEDS exceeded\n");
				break;
			}
		}
	}
	return nseeds;
}

void
applymaps(Seed seeds[], int nseeds, const struct map maps[], int nmaps)
{
	int i, j;
	for (i = 0; i < nseeds; i++) {
		for (j = 0; j < nmaps; j++) {
			if (inwindow(seeds[i], maps[j].src, maps[j].len)) {
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

