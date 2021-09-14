int b[5];
int main()
{
    int a[10];

    for( int i = 0;  i < 10;  i++ ){
        a[i] = i*i;
	b[i] = i;
    }

    for( int i = 0; i < 10; i++ ){
        printint( a[i] );
        printint( b[i] );
    }
}
