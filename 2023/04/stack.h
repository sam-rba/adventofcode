struct stnode {
	int data;
	struct stnode *next;
};
typedef struct stnode Stack;

Stack *stpush(Stack *st, const int data);
Stack *stpop(Stack *st, int *data);
void stprint(Stack *st);
void stfree(Stack **st);

