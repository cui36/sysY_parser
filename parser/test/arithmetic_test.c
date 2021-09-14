int a;
int fn1( int a )
{
    return a + 1;
}

int main()
{
    int b = 2;
    
    a = 2 + b - 3 + 7;   //8
    printint(a);
    
    a = b * 2 + 3 * 4 / 2;  //10
    printint(a);

    a = a / 2;
    printint(a);   // 5

    a = 11 % ( 1 + b );   //2
    printint(a);

    printint( fn1( 2 * b ) ); //5

    b = a = 9;  
    printint(b);  //9

    b = a + b * 2;
    printint(b);   //27

    return 1;
}
