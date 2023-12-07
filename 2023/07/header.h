#define CARDS 5 /* cards per hand */
#define NCARDTYPES 14

#define FNAME "/dev/stdin"
#define MAXLINE 16
#define MAXHANDS 1024

typedef enum {
	HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND,
	FULL_HOUSE, FOUR_OF_A_KIND, FIVE_OF_A_KIND
} HandType;

typedef enum {
	JOKER,
	TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN,
	JACK, QUEEN, KING, ACE
} Card;

struct hand {
	Card cards[CARDS];
	int bid;
};

void parsehand(struct hand *h, const char line[]);
Card parsecard(char c);
int isdigit(char c);
int cmphand(const void *p1, const void *p2);
HandType handtype(struct hand *h);
int rcmpint(const void *p1, const void *p2);

