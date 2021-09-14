%option noyywrap nodefault yylineno

%{
#include "decl.h"
#include "defs.h"
#include "data.h"

#define YY_USER_ACTION  yylloc.first_line = yylloc.last_line = yylineno;\
          yylloc.first_column = yycolumn;  yylloc.last_column+yyleng-1; \
          yycolumn += yyleng;



int yycolumn = 0;
std::string yystring;


static void comment();
static int lex_char_esc( char *str );
static int charToDec();
static std::string getString();
static int getChar();
static char* getIdentifier();

%}


L    [a-zA-Z_]
D    [0-9]    
H    [a-fA-F0-9]
  /* char escape  转义字符 */
CE   n|t|v|b|r|f|a|\\|\?|\"|\'|[0-7]+|x{H}+


%x STRING_X


%%
"/*"                    { comment(); }
"//"[^\n]*              { }


"string"                { return STRING; }
"char"                  { return CHAR; }
"short"			{ return(SHORT); }
"int"			{ return(INT); }
                                         
"void"			{ return(VOID); }
"return"		{ return(RETURN); }

"continue"              { return (CONTINUE);}
"break"                 { return (BREAK); }
"do"                    { return (DO); }
"while"			{ return(WHILE); }
"if"                    { return (IF); }
"else"                  { return (ELSE);}
"for"                   { return (FOR); }

"goto"                  { return (GOTO); }

"struct"		{  return(STRUCT); }


{L}({L}|{D})*		{  yylval.astNode = identASTnode(getIdentifier()); 
                           return (IDENTIFIER); }

\'.\'                   {  yylval.astNode = constASTnode(getChar()); 
                           return(CONSTANT); }
\'\\{CE}\'              {  yylval.astNode = constASTnode(getChar()); 
                           return(CONSTANT); }

0[xX]{H}+		{  yylval.astNode = constASTnode(charToDec()); 
                           return(CONSTANT); }
0[0-7]*		        {  yylval.astNode = constASTnode(charToDec()); 
                           return(CONSTANT); }
[1-9]{D}*		{  yylval.astNode = constASTnode(charToDec()); 
                           return(CONSTANT); }

\"              	{ BEGIN STRING_X; yystring.clear(); }
<STRING_X>[^\"\n]+      { yystring += yytext; }
<STRING_X>\\{CE}        { yystring.push_back( lex_char_esc(&yytext[1]) ); }
<STRING_X>\"            { yylval.astNode = strASTnode(getString()); 
                          BEGIN INITIAL;  return(STRING_LITERAL); }
<STRING_X>\n            { BEGIN INITIAL; 
                          //多读了一个换行符，行数减回1
                          Perror(emEC_Miss_Terminating_character, yylineno-1, '\"'); }


">>"			{  return(RIGHT_OP); }
"<<"			{  return(LEFT_OP); }
"++"			{  return(INC_OP); }
"--"			{  return(DEC_OP); }
"&&"			{  return(AND_OP); }
"||"			{  return(OR_OP); }

"<="			{  return(LE_OP); }
">="			{  return(GE_OP); }
"=="			{  return(EQ_OP); }
"!="			{  return(NE_OP); }


"&"                     {  return('&'); }
";"			{  return(';'); }
"{"	        	{  return('{'); }
"}"    		        {  return('}'); }
","			{  return(','); }
"="			{  return('='); }
"("			{  return('('); }
")"			{  return(')'); }
"["	        	{  return('['); }
"]"		        {  return(']'); }
"."			{  return('.'); }
"-"			{  return('-'); }
"+"			{  return('+'); }
"*"			{  return('*'); }
"/"			{  return('/'); }
"%"			{  return('%'); }
"<"			{  return('<'); }
">"			{  return('>'); }

[ \t\v\f]		{ ; }
\n                      { yycolumn = 1; }
<<EOF>>                 { yyterminate(); }
.                       {PerrorWithLineNo(emEC_Unknown_character, *yytext); }



%%


/*
略过注释部分的内容
*/
void comment()
{
    char c, prev = 0;
    while( c = yyinput() ){
        if( c == '/'  &&  prev == '*' )
            return;
        prev = c;
    }

   //块注释不完整报错
   Perror(emEC_Unterminated_comment, yylineno-1); 
}



/* 
将转义字符转化为对应的ASCII值
参数str格式为{CE}
CE   n|t|v|b|r|f|a|\\|\?|\"|\'|[0-7]+|x{H}+
*/
int lex_char_esc( char *str )
{
    int a, c = 0;
    char *p;

    a = str[0];
    switch( a ){
        case 'n' : c = '\n'; break;
        case 't' : c = '\t'; break;
        case 'v' : c = '\v'; break;
        case 'b' : c = '\b'; break;
        case 'r' : c = '\r'; break;
        case 'f' : c = '\f'; break;
        case 'a' : c = '\a'; break;
        case '\\' : c = '\\'; break;
        case '\?' : c = '\?'; break;
        case '\"' : c = '\"'; break;
        case '\'' : c = '\''; break;
        case 'x'  : {
            p = &str[1];
            while( p ){
                if( *p >= '0' && *p <= '9' )
                    c = c*16 + *p - '0';
                else if ( (tolower(*p)>='a' && tolower(*p)<='f') )
                    c = c*16 + *p - 'a' + 10;
                else
                    break;
                p++;
            }
            break;
        }
        default:{
            if( a >= '0' &&  a <= '7' ){
                c = a - '0';
                p = &str[1];
                while( *p >= '0'  &&  *p <= '7' ){
                    c = c*8 + (*p - '0');
                    p++;
                }
            }
            break; 
        }
    }

    return c;
}





/*
将字符数字转为对应的数值
0[xX]{H}+	
0[0-7]*		
[1-9]{D}*	
*/
int charToDec()
{
    int val = 0;
    char *c = yytext;
    if( *c == '0' ){
        c++;
        if( *c == 'x' ||  *c == 'X' ){   //16进制
            c++;
            while( *c ){
                if( isalpha(*c) )
                    val = val*16 + tolower(*c++) - 'a' + 10;
                else
                    val = val*16 + *c++ - '0';
            }
        } 
        else {  // 8进制
            while( *c ){
                val = val*8 + *c++ - '0';
            }
        }
    }
    else{  //十进制
        while( *c )
            val = val*10 + *c++ - '0';
    }

    return val; 
}




/*
获取字符对应的ASCII值
字符有如下2种形式
\'.\'     
\'\\{CE}\'
*/
int getChar()
{
    int val;
    char *c = yytext;

    c++;
    if( *c = '\\' )   //转义字符
        val = lex_char_esc( ++c );
    else
        val = *c;
    
    return val;
}



/*
获取字符串内容
*/
std::string getString()
{
    return yystring;
}



/*
获取标识符
标识符形式 : {L}({L}|{D}*
*/
char* getIdentifier()
{
    return yytext;
}


