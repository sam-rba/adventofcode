#include <stdio.h>

#include "header.h"

#define SEEDRANGES 64

struct seedrange {
	Seed start;
	unsigned int len;
};

int readseedranges(struct seedrange seeds[], const char line[]);
int applymaps(struct seedrange seeds[], int nseeds, const struct map maps[], int nmaps);
void splitrange(struct seedrange *dst, struct seedrange *src, int i);
Seed min(const struct seedrange seeds[], int nseeds);

int
main()
{
	FILE *file;
	char line[MAXLINE];
	struct seedrange seeds[SEEDRANGES];
	int nseeds;
	int i;
	struct map maps[MAPS], *mapp;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}

	if (fgets(line, MAXLINE, file) == NULL
			|| (nseeds = readseedranges(seeds, line)) < 1) {
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
			nseeds = applymaps(seeds, nseeds, maps, mapp-maps);
			if (nseeds < 0) {
				fclose(file);
				return 1;
			}
			mapp = maps;
		}
	}
	if ((nseeds = applymaps(seeds, nseeds, maps, mapp-maps)) >= 0)
		printf("Part 2: %lu\n", min(seeds, nseeds));

	fclose(file);
	return 0;
}

int
readseedranges(struct seedrange seeds[], const char line[])
{
	int i, n;

	for (i = 0; line[i] != '\0' && line[i] != ':'; i++)
		;
	if (line[i] == '\0')
		return -1;

	n = 0;
	seeds[n].start = seeds[n].len = 0;
	while (line[++i] != '\0') {
		if (isdigit(line[i])) {
			if (seeds[n].start == 0) {
				for (; isdigit(line[i]); i++)
					seeds[n].start = (seeds[n].start * 10) + (line[i] - '0');
			} else {
				for (; isdigit(line[i]); i++)
					seeds[n].len = (seeds[n].len * 10) + (line[i] - '0');
				if (++n < SEEDRANGES) {
					seeds[n].start = seeds[n].len = 0;
				} else {
					printf("SEEDRANGES exceeded\n");
					break;
				}
			}
		}
	}
	return n;
}

int
applymaps(struct seedrange seeds[], int nseeds, const struct map maps[], int nmaps)
{
	int i, j;

	for (i = 0; i < nseeds; i++) {
		for (j = 0; j < nmaps; j++) {
			if (maps[j].src <= seeds[i].start
					&& seeds[i].start < maps[j].src + maps[j].len) {
				if (seeds[i].start + seeds[i].len >= maps[j].src + maps[j].len) {
					if (++nseeds < SEEDRANGES) {
						splitrange(&seeds[nseeds-1], &seeds[i],
								maps[j].src + maps[j].len);
					} else {
						printf("SEEDRANGES exceeded\n");
						return -1;
					}
				}
				seeds[i].start = seeds[i].start - maps[j].src + maps[j].dst;
				break;
			}
		}
	}
	return nseeds;
}

void
splitrange(struct seedrange *dst, struct seedrange *src, int i)
{
	dst->start = i;
	dst->len = src->start + src->len - i;
	src->len -= dst->len;
}

Seed
min(const struct seedrange seeds[], int nseeds)
{
	Seed min;
	int i;

	min = ~0L;
	for (i = 0; i < nseeds; i++)
		min = (seeds[i].start < min) ? seeds[i].start : min;
	return min;
}

