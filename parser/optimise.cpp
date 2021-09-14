#include "decl.h"
#include "defs.h"
#include "data.h"

/*
 a = b + c
 a(lacal)    => IR_BPREL(for a)
 b + c -> a  => IR_STORE(for a)

 d = a       => IR_LOAD (for a)
 * */

void optimize( IR *ir )
{
    if( ir->var && ir->var->take_addr == true )
	return;

    if( ir->op == IR_BPREL ){  //左值
        if( ir->var->type->ty == emTYPE_INT || ir->var->type->ty == emTYPE_SHORT
		|| ir->var->type->ty == emTYPE_CHAR){   //不包括指针，数组
	    if( ir->var->promoted == NULL )   //内存读取改为寄存器读取
	       ir->var->promoted = newReg();  //对于同一个变量对应同一个寄存器

	    ir->op = IR_NOP;
            ir->r0->promoted = ir->var->promoted;
	}
    }
    else if( ir->op == IR_STORE ){  
	if( ir->r1->promoted ){
	    ir->op = IR_MOV;
	    ir->r0 = ir->r1->promoted;
	    ir->r1 = NULL;
	}
    }
    else if( ir->op == IR_LOAD ){
        if( ir->r2->promoted ){
	    ir->op = IR_MOV;
	    ir->r2 = ir->r2->promoted;
	}
    }
    else if( ir->op == IR_SCALE ){     //类型缩放
        if( ir->r1->promoted ){
	    ir->r1 = ir->r1->promoted;
	}
    }
}


void opt()
{
    for( int i = 0;  i < prog.gFuncs.size();  i++ ){
	Function *fn = prog.gFuncs[i];
        for( int i = 0;  i < fn->bbs.size();  i++ ){
	    BB *bb = fn->bbs[i];
	    for( int i = 0;  i < bb->vec_ir.size();  i++ ){
		IR *ir = bb->vec_ir[i];
	        optimize( ir );
	    }
	}
    }
}
