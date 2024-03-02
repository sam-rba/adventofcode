struct stack {
	char *record;
	struct stack *next;
};

struct qnode {
	char *record;
	struct qnode *next;
};

struct queue {
	struct qnode *head;
	struct qnode *tail;
};

struct stack *stpush(struct stack *stk, const char *record);
struct stack *stpop(struct stack *stk, char **record);
void stfree(struct stack **stk);

void enqueue(struct queue *q, const char *record);
char *dequeue(struct queue *q);
void qfree(struct queue *q);

