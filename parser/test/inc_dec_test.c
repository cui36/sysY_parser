int main()
{
    int a = 1;
    int b;

    b = -a;
    printint(b);   //-1

    b = a++;
    printint(a);  //2
    printint(b);  //1

    b = a--;
    printint(a);   //1
    printint(b);  //2

    b = +a;
    printint(b);   //1

    b = ++a;
    printint(a);    //2
    printint(b);    //2

    b = --a;
    printint(b);    //1

    int c = 9;
    a = 3;
    b = (a--) + (-c) ;
    printint(b);    //-6

}
