#include "decl.h"
#include "defs.h"
#include "data.h"




void error( const char *format,  ... )
{
    ErrorCnt++;
    va_list args;

    va_start(args, format);
    vfprintf( stderr, format, args );
    va_end(args);

    fprintf(stderr, "\n");
    exit(0);
}


void error( const ERROR_CODE errorCode,  ... )
{
    ErrorCnt++;
    va_list args;

    va_start(args, errorCode);
    vfprintf( stderr, ErrorInfo[errorCode], args );
    va_end(args);

    fprintf(stderr, "\n");

    exit(0);
}

//new

void error( const ERROR_CODE errorCode, int lineno,  ... )
{
    ErrorCnt++;
    va_list args;

    fprintf( stderr, "line %d : ", lineno );

    va_start(args, lineno);
    vfprintf( stderr, ErrorInfo[errorCode], args );
    va_end(args);

    fprintf(stderr, "\n");

    exit(0);
}



void errorWithLineNo( const ERROR_CODE errorCode, ... )
{
    ErrorCnt++;
    va_list args;

    fprintf( stderr, "line %d : ", yylineno );
    va_start(args, errorCode);
    vfprintf( stderr, ErrorInfo[errorCode], args );
    va_end(args);

    fprintf(stderr,"\n");

    exit(0);
}





void Perror( const char *format,  ... )
{
    ErrorCnt++;
    va_list args;

    va_start(args, format);
    vfprintf( stderr, format, args );
    va_end(args);

    fprintf(stderr, "\n");
}


/*
 * 输出错误信息前，参数lineno可以指定需要输出的行号
 * */
void Perror( const ERROR_CODE errorCode, int lineno,  ... )
{
    ErrorCnt++;
    va_list args;

    fprintf( stderr, "line %d : ", lineno );

    va_start(args, lineno);
    vfprintf( stderr, ErrorInfo[errorCode], args );
    va_end(args);

    fprintf(stderr,"\n");
}



/*
 * 输出错误信息前，自动输出当前行号
 * */
void PerrorWithLineNo( const ERROR_CODE errorCode, ... )
{
    ErrorCnt++;
    va_list args;

    fprintf( stderr, "line %d : ", yylineno );
    va_start(args, errorCode);
    vfprintf( stderr, ErrorInfo[errorCode], args );
    va_end(args);

    fprintf(stderr, "\n");
}
