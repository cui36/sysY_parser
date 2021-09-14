#include "decl.h"
#include "defs.h"
#include "data.h"


static Type* newType( int type, int size )
{
    Type *ty = new Type();
    ty->ty = type;
    ty->size = ty->align = size;
    ty->ary_of = ty->ptr_to = NULL;

    return ty;
}


static Type* newType( int type, int size, int align )
{
    Type *ty = new Type();
    ty->ty = type;
    ty->size = size;
    ty->align = align;
    ty->ary_of = ty->ptr_to = NULL;

    return ty;
}


Type* array_type( Type *base, int len )
{
    int size = len * base->size;
    int align = 1;
    if( size >= 32 )
	align = 32;
    else if( size >= 16 )
	align = 16;
    else if( size >= 8 )
	align = 8;
    size = ( size + align - 1 ) & (~( align - 1 ));  //对齐

    Type *ty = newType( emTYPE_ARRAY, size, align );
    ty->ary_of = base;

    return ty;
}


Type* string_type( )
{
    return newType( emTYPE_STRING, 0 );
}


Type* pointer_type( Type *base )
{
    Type *ty = newType( emTYPE_POINTER, 8, 8 );
    ty->ptr_to = base;
    return ty;
}


Type* void_type( )
{
    return newType( emTYPE_VOID, 0 );
}


Type* char_type()
{
    return newType( emTYPE_CHAR, 1 );
}


Type* short_type()
{
    return newType( emTYPE_SHORT, 2 );
}


Type* int_type()
{
    return newType( emTYPE_INT, 4 );
}



