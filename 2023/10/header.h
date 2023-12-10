/* maximum grid dimensions */
#define MAXW 192
#define MAXH 192

typedef enum {
	NS = '|', EW = '-', NE = 'L', NW = 'J', SW = '7', SE = 'F',
	GROUND = '.', START = 'S'
} Tile;

typedef enum {
	NORTH, EAST, SOUTH, WEST
} Direction;

struct coords {
	int x;
	int y;
};

int readgrid(Tile grid[MAXH][MAXW], struct coords *size);
int cyclelen(const Tile grid[MAXH][MAXW], struct coords size);
int startpos(struct coords *pos, const Tile grid[MAXH][MAXW], struct coords size);
Direction choosedir(const Tile grid[MAXH][MAXW], struct coords size, struct coords *pos);
Direction move(const Tile grid[MAXH][MAXW], struct coords size, struct coords *pos, Direction dir);
int accessiblefrom(Tile tile, Direction dir);
Direction outdir(Tile tile, Direction indir);
void printdir(Direction dir);

