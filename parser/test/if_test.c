int main()
{
    int a = 1;
    if( a > 2 ){
	a++;
        printint(a);
    }else{
        a--;
        printint(a);  //0
    }


    if( a + 3 > 4 * a ){
        printint(a);   //0
    }
}

