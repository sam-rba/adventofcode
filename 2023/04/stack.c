#include <stdlib.h>
#include <stdio.h>

#include "header.h"

Stack *
stpush(Stack *st, const int data)
{
	struct stnode *head;

	head = (struct stnode *) malloc(sizeof(struct stnode));
	head->data = data;
	head->next = st;
	return head;
}

Stack *
stpop(Stack *st, int *data)
{
	struct stnode *head;

	if (st == NULL)
		return st;

	*data = st->data;
	head = st->next;
	free(st);
	return head;
}

void
stfree(Stack **st)
{
	if (*st == NULL)
		return;
	stfree(&((*st)->next));
	free(*st);
	*st = NULL;
}

