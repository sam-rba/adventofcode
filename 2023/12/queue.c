#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "header.h"

void
enqueue(struct queue *q, const char *record) {
	struct qnode *n;
	int len;

	if (q == NULL) {
		return;
	}

	n = (struct qnode *) malloc(sizeof(struct qnode));
	if (n == NULL) {
		printf("failed to allocate queue node\n");
		return;
	}
	len = strlen(record)+1; // +1 for \0 for strlcpy
	n->record = (char *) calloc(len, sizeof(char));
	if (n->record == NULL) {
		printf("failed to allocate space for record on queue\n");
		free(n);
		return;
	}
	(void) strlcpy(n->record, record, len);
	n->next = NULL;

	if (q->head == NULL) {
		q->head = q->tail = n;
	} else {
		q->tail = q->tail->next = n;
	}
}

char *
dequeue(struct queue *q) {
	struct qnode *n;
	char *record;

	if (q->head == NULL) {
		return NULL;
	}

	n = q->head;
	q->head = q->head->next;
	if (q->head == NULL) {
		q->tail = NULL;
	}
	record = n->record;
	free(n);
	return record;
}

void
qfree(struct queue *q) {
	char *record;
	while ((record = dequeue(q)) != NULL) {
		free(record);
	}
}

