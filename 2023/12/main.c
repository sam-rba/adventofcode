#include <stdio.h>
#include <stdlib.h>

#include "header.h"

#define MAX_GROUPS 16
#define RECORD_LEN 32

char *getrecord();
int getgroups(int *groups);
void allperms(struct queue *q, char *record);
int groupsmatch(const char *record, const int *groups, int ngroups);
int prefixlen(const char *str, char c);
int isnum(char c);
int contains(const char *str, char c);

int
main() {
	struct queue q;
	char *record;
	int groups[MAX_GROUPS];
	int recordlen, ngroups, count;

	count = 0;
	q.head = q.tail = NULL;
	while ((record = getrecord()) != NULL) {
		if ((ngroups = getgroups(groups)) < 1) {
			printf("not enough groups\n");
			return 1;
		}
		allperms(&q, record);
		free(record);
		while ((record = dequeue(&q)) != NULL) {
			if (groupsmatch(record, groups, ngroups)) {
				count++;
			}
			free(record);
		}
		printf("count=%d\n", count);
	}
	qfree(&q);
	printf("p1: %d\n", count);

	return 0;
}

char *
getrecord() {
	char *record;
	int c, i;

	record = (char *) calloc(RECORD_LEN, sizeof(char));
	if (record == NULL) {
		printf("failed to allocate record\n");
		return NULL;
	}

	i = 0;
	while ((c = getchar()) != ' ' && c != EOF) {
		if (i >= RECORD_LEN-1) {
			printf("RECORD_LEN exceeded\n");
			break;
		}
		record[i++] = (char) c;
	}
	if (c == EOF) {
		free(record);
		return NULL;
	}
	record[i] = '\0';
	return record;
}

int
getgroups(int groups[MAX_GROUPS]) {
	int i, c;

	groups[i = 0] = 0;
	while ((c = getchar()) != '\n' && c != EOF) {
		if (isnum(c)) {
			groups[i] = groups[i] * 10 + (c-'0');
			continue;
		}
		if ((++i) > MAX_GROUPS) {
			printf("MAX_GROUPS exceeded\n");
			return i-1;
		}
		groups[i] = 0;
	}
	return i+1;
}

// horribly inefficient
// TODO: use trie(?) rather than this malarky
void
allperms(struct queue *q, char *record) {
	int i;

	enqueue(q, record);
	do {
		record = dequeue(q);
		for (i = 0; record[i] != '\0'; i++) {
			if (record[i] == '?') {
				record[i] = '#';
				enqueue(q, record);
				record[i] = '.';
				enqueue(q, record);
				break;
			}
		}
		if (record[i] == '\0') {
			enqueue(q, record);
			free(record);
			return;
		}
		free(record);
	} while (1);
}

int
groupsmatch(const char *record, const int *groups, int ngroups) {
	int i, size;

	for (i = 0; i < ngroups; i++) {
		record += prefixlen(record, '.');
		if ((size = prefixlen(record, '#')) != groups[i]) {
			return 0;
		}
		record += size;
	}
	return !contains(record, '#');
}

int
prefixlen(const char *str, char c) {
	int len;

	for (len = 0; *str == c; len++) {
		str++;
	}
	return len;
}

int
isnum(char c) { return '0' <= c && c <= '9'; }

int
contains(const char *str, char c) {
	while (*str != '\0') {
		if (*str == c) {
			return 1;
		}
		str++;
	}
	return 0;
}

