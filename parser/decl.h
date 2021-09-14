#ifndef _DELCS_H_
#define _DELCS_H_

// c
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
// c++
#include <string>
#include <vector>
#include <map>
//self
#include "parse.tab.h"
#include "defs.h"


/***** error.cpp *******/
void PerrorWithLineNo( const ERROR_CODE errorCode,  ... );
void Perror( const ERROR_CODE errorCode, int lineno, ... );
void Perror( const char *format,  ... );
void errorWithLineNo( const ERROR_CODE errorCode,  ... );
void error( const ERROR_CODE errorCode, int lineno, ... );
void error( const ERROR_CODE errorCode,  ... );
void error( const char *format,  ... );


/* parse.y */
ASTnode* identASTnode( const char *name  );
ASTnode* strASTnode( std::string str );
ASTnode* constASTnode( int val );



/* lex.l */
int yylex();


/* type.cpp */
Type* void_type( );
Type* char_type();
Type* short_type();
Type* int_type();
Type* pointer_type( Type *base );
Type* array_type( Type *base, int len );
Type* string_type();


/* semantic.cpp */
void parseASTTree( ASTnode *n );
Var* initVar( std::string name, Type *ty, bool is_local);
Function* lookforFunc( std::string name );


/* gen_ir.cpp */
void gen_ir();
Reg* newReg();


/* optimise.cpp */
void opt();

/* regalloc.cpp */
void alloc_regs();


/* gen_x86.cpp */
void gen_x86( );


#endif
