int test2( int a )
{
    int b = 3;
    printint(a);
    return b;
}
int test1( int a )
{
    int b = 2;
    printint(b);
    return a;
}

int test3( int a )
{
    printint(a);
    return a;
}


void main()
{
    test2(4);  //4
    test2(2);  //2
    test3( test2( test1(10) ) ); // 2 10 3
}
