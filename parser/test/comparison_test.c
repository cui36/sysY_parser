int main()
{
    int a = 1 > 2;
    printint( a );   //0

    a = 1 >= 2;
    printint( a );   //0

    int b = 2;
    a = b * 2 < 8 / 2;
    printint( a );   //0

    a = b * 2 <= 8 / 2;
    printint( a );   //1

    a = b == 2;
    printint( a );   //1

    a = b != 2;
    printint( a );   //0

}
