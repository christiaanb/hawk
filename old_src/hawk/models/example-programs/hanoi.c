
/* Maximum tower height */
#define TOWER_HEIGHT 5


/* The type of a tower */
typedef struct
{
	int pieces[TOWER_HEIGHT]; /* stack of pieces, the bottom-most being */
				  /*  at index zero */
	int topIndex;		  /* Index of the top piece of the tower. */
				  /*  if topIndex == -1, then the tower is empty */
} Tower;


/* The actual towers */
Tower towerA,towerB,towerC;

/* Counts how many times any piece was moved from one tower to another */
int piecesMoved;

/* At the moment, the Hawk parser requires main() to be the first
**  defined function in the file
*/
main()
{
	runHanoi();
}


/* Initializes the three towers, so that "a" is full,
**  and the other two towers are empty
*/
initTowers( Tower* a, Tower* b, Tower* c )
{
	int i;

	piecesMoved = 0;

	for( i = 0; i < TOWER_HEIGHT; ++i )
	{
		a->pieces[i] = TOWER_HEIGHT - i;
	}
	a->topIndex = TOWER_HEIGHT - 1;

	b->topIndex = -1;
	c->topIndex = -1;
}


/* Returns 1 if the tower is full, and properly stacked */
int towerIsFull (Tower* t)
{
	int piecesInOrder = 1;
	int i;

	for( i = 0; i < TOWER_HEIGHT; ++i)
	{
		if( t->pieces[i] != (TOWER_HEIGHT - i) )
		{
			piecesInOrder = 0;
			break;
		}
	}

	return( piecesInOrder && (t->topIndex == TOWER_HEIGHT - 1) );
}


/* Moves a single piece from one tower to another. */
void movePiece( Tower* from, Tower* to )
{
	int piece;

	piece = from->pieces[(from->topIndex)--];
	to->pieces[++(to->topIndex)] = piece;

	piecesMoved++;
}


/* Moves a stack of n pieces from one tower to another, using
**  the third tower as a temporary holding place
*/
void moveStack( int n, Tower* from, Tower* to, Tower* tmp )
{
	if (n == 0)
	{
		return;
	}
	else
	{
		moveStack( n - 1, from, tmp, to );
		movePiece( from, to );
		moveStack( n - 1, tmp, to, from );
	}
}
		

runHanoi()
{
	int result;

	initTowers( &towerA, &towerB, &towerC );

	moveStack( TOWER_HEIGHT, &towerA, &towerB, &towerC);

	if( towerIsFull( &towerB) )
	{
		/* The program worked! */
		result = 888;
		printf(0,result,piecesMoved); 
	}
	else
	{
		/* Bad program, bad! */
		result = 666;
		printf(0,result,piecesMoved);
	}
}
