void fn( char c )
{
    printint(c);
}


int intArr[2][3];
char chrArr[2][3];
short s = 77;
char chr = 300;

int main()
{
	int a = 10000;
	char c = 11;
	short b = 99;       //赋值需要
	int r = a + b + c;  //表达式需要扩充
	printint(r);   //10110
	
	int i = 999;
	c = i;
	printint(c);  //-25 测试溢出
	fn(i);        //-25 测试溢出

	c = 22;
	i = c;
	printint(i);  //22

	for( int i = 0;  i < 2; i++ ){
	    for( int j = 0;  j < 3; j++ ){
	        intArr[i][j] = j + i;
		chrArr[i][j] = intArr[i][j];
		printint(chrArr[i][j]);
	    }
	}

	printint(s);    //77
	printint(chr);  //44  测试溢出

	chrArr[0][1] = 300;
	printint(chrArr[0][1]);   //44
}
