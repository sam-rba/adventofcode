#include <stdio.h>

#define MAXSTEP 16

int readstep(char step[]);
unsigned char hash(const unsigned char step[]);

int
main()
{
	unsigned char step[MAXSTEP];
	unsigned int sum;

	sum = 0;
	while (readstep(step) > 0) {
		sum += hash(step);
	}
	printf("part 1: %u\n", sum);

	return 0;
}

int
readstep(char step[])
{
	int i, c;

	for (i = 0; i < MAXSTEP && (c = getchar()) != EOF; i++) {
		if (c == ',' || c == '\n') {
			break;
		}
		step[i] = c;
	}
	if (i >= MAXSTEP) {
		printf("MAXSTEP exceeded\n");
		i--;
	}
	step[i] = '\0';
	return i;
}

unsigned char
hash(const unsigned char step[])
{
	unsigned char v;

	v = 0;
	for (; *step != '\0'; step++) {
		v += *step;
		v *= 17;
		v %= 256;
	}
	return v;
}

