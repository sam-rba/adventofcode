#include <stdio.h>
#include <string.h>

#include "header.h"

/* maximum number of lenses per box */
#define LENSES 8
/* maximum length of label including terminator */
#define LABEL 8
/* number of boxes */
#define BOXES 256

typedef enum {
	REMOVE = '-', SET = '='
} Operation;

struct step {
	char label[LABEL];
	Operation op;
	unsigned char focallen;
};

struct lens {
	char label[LABEL];
	unsigned char focallen;
};

struct box {
	struct lens lenses[LENSES];
	int nlenses;
};

void setlens(struct box *bx, const char *label, char focallen);
void removelens(struct box *bx, const char *label);
void readstep(struct step *stp);

int
main()
{
	struct box boxes[BOXES];
	struct step stp;
	unsigned char boxid;
	unsigned int i, j, power;

	for (i = 0; i < BOXES; i++)
		boxes[i].nlenses = 0;

	for (readstep(&stp); stp.label[0] != '\0'; readstep(&stp)) {
		boxid = hash(stp.label);
		switch (stp.op) {
		case REMOVE:
			removelens(&boxes[boxid], stp.label);
			break;
		case SET:
			setlens(&boxes[boxid], stp.label, stp.focallen);
			break;
		default:
			printf("invalid operation: %c\n", stp.op);
		}
	}

	power = 0;
	for (i = 0; i < BOXES; i++) {
		for (j = 0; j < boxes[i].nlenses; j++) {
			power += (i+1) * (j+1) * (unsigned int) boxes[i].lenses[j].focallen;
		}
	}
	printf("part 2: %u\n", power);

	return 0;
}

void
readstep(struct step *stp)
{
	int i, c;

	/* label */
	for (i = 0; i < LABEL && (c = getchar()) != EOF; i++) {
		if (c < 'a' || c > 'z')
			break;
		stp->label[i] = c;
	}
	if (i >= LABEL) {
		printf("LABEL exceeded\n");
		i--;
	}
	stp->label[i] = '\0';

	/* operation */
	switch (c) {
	case EOF:
		return;
	case REMOVE:
		stp->op = c;
		break;
	case SET:
		stp->op = c;
		if ((c = getchar()) < '0' || c > '9') {
			printf("invalid focal len: %c\n", c);
		} else {
			stp->focallen = (unsigned char) c-'0';
		}
		break;
	default:
		printf("readstep invalid operation: %c\n", c);
	}
	(void) getchar();
}

void
removelens(struct box *bx, const char *label)
{
	int i;

	for (i = 0; i < bx->nlenses; i++) {
		if (strcmp(bx->lenses[i].label, label) == 0) {
			for (i++; i < bx->nlenses; i++) {
				bx->lenses[i-1] = bx->lenses[i];
			}
			bx->nlenses--;
			return;
		}
	}
}

void
setlens(struct box *bx, const char *label, char focallen)
{
	int i;

	for (i = 0; i < bx->nlenses; i++) {
		if (strcmp(bx->lenses[i].label, label) == 0) {
			bx->lenses[i].focallen = focallen;
			return;
		}
	}
	if (i >= LENSES) {
		printf("LENSES exceeded\n");
		return;
	}
	strncpy(bx->lenses[bx->nlenses].label, label, LABEL);
	bx->lenses[bx->nlenses++].focallen = focallen;
}

