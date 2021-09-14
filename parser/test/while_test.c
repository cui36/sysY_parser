int main()
{
    int a = 1;
    while( a < 4 ){
	printint(++a);  // 2 3 4
    }

    a = 1;
    while( a < 4 ){
	printint(a++);   
    }

    a = 4;
    while( a > 1 ){
	printint(--a);
    }

    a = 4;
    while( a > 1 ){
	printint(a--);
    }
}
