int a;
int main()
{
    int b = 2;
    a = b = 9;
    printint(a); // 9
    printint(b); // 9

    int c = a = b * 2;
    printint(a); // 18
    printint(c); //18

    a = 0xa;
    printint(a);   //10

    return 1;
}
