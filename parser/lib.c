#include<stdio.h>
void printint( int val )
{
    printf("%d\n", val);
}

void printstr( char *s )
{
    printf("%s", s);
}

void readint( int *val )
{
    char c;
    scanf("%d", val);
    while( c = getchar() != '\n' );
}

