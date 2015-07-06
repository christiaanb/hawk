/*

map fibbo [1 .. 10] = [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]


*/
int fibbo(int);

main()
{
	int x;
	x = fibbo(5);
        if (x==8) printf(0,888);
        else printf(0,666);
}

int fibbo(int x)
{
	if (x<2) return 1;
	else return fibbo(x-1) + fibbo(x-2);
}
