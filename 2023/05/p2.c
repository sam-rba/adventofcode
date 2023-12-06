#include <stdio.h>

#include "header.h"

#define SEEDRANGES 64

struct seedrange {
	Seed start;
	unsigned int len;
};

int applymaps(struct seedrange seeds[], int nseeds, const struct map maps[], int nmaps);
int readseedranges(struct seedrange seedranges[], const char line[]);
Seed min(const struct seedrange seedranges[], int nseedranges);

/* bruteforce */
int
main()
{
	FILE *file;
	char line[MAXLINE];
	struct seedrange seedranges[SEEDRANGES];
	int nseedranges;
	int i;
	struct map maps[MAPS], *mapp;

	if ((file = fopen(FNAME, "r")) == NULL) {
		printf("failed to open %s\n", FNAME);
		return 1;
	}

	if (fgets(line, MAXLINE, file) == NULL
			|| (nseedranges = readseedranges(seedranges, line)) < 1) {
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
			nseedranges = applymaps(seedranges, nseedranges, maps, mapp-maps);
			if (nseedranges < 0) {
				fclose(file);
				return 1;
			}
			mapp = maps;
		}
	}
	if ((nseedranges = applymaps(seedranges, nseedranges, maps, mapp-maps)) >= 0)
		printf("Part 1: %lu\n", min(seedranges, nseedranges));

	fclose(file);
	return 0;
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
					/* split seed range */
					if (++nseeds < SEEDRANGES) {
						seeds[nseeds-1].start = maps[j].src + maps[j].len;
						seeds[nseeds-1].len = (seeds[i].start + seeds[i].len)
														- (maps[j].src + maps[j].len);
						seeds[i].len -= seeds[nseeds-1].len;
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

int
readseedranges(struct seedrange seedranges[], const char line[])
{
	int i, n;

	for (i = 0; line[i] != '\0' && line[i] != ':'; i++)
		;
	if (line[i] == '\0')
		return -1;

	n = 0;
	seedranges[n].start = seedranges[n].len = 0;
	while (line[++i] != '\0') {
		if (isdigit(line[i])) {
			if (seedranges[n].start == 0) {
				for (; isdigit(line[i]); i++)
					seedranges[n].start = (seedranges[n].start * 10) + (line[i] - '0');
			} else {
				for (; isdigit(line[i]); i++)
					seedranges[n].len = (seedranges[n].len * 10) + (line[i] - '0');
				if (++n < SEEDRANGES) {
					seedranges[n].start = seedranges[n].len = 0;
				} else {
					printf("SEEDRANGES exceeded\n");
					break;
				}
			}
		}
	}
	return n;
}

Seed
min(const struct seedrange seedranges[], int nseedranges)
{
	Seed min;
	int i;

	min = ~0L;
	for (i = 0; i < nseedranges; i++)
		min = (seedranges[i].start < min) ? seedranges[i].start : min;
	return min;
}

