#include "decl.h"
#include "defs.h"
#include "data.h"




IR* newIR( int op )
{
    IR *ir = new IR();
    ir->op = op;
    ir->r0 = ir->r1 = ir->r2 = NULL;

    return ir;
}



// A = B op C;  =>  A = B;  A = A op C;
// r0 = r1 op r2;
void SSA_to_x86( BB *bb )
{
    std::vector<IR*> x86_IRs;

    for( int i = 0;  i < bb->vec_ir.size();  i++ ){
        IR *ir = bb->vec_ir[i];
        
	//r0, r1i其中一个为空，则该指令不是3地址指令
	if( !ir->r0 || !ir->r1 ){
	    x86_IRs.push_back(ir);
	    continue;
	}

	IR *ir2 = newIR(IR_MOV);
        ir2->r0 = ir->r0;  //共用寄存器
	ir2->r2 = ir->r1;
	x86_IRs.push_back(ir2);

	ir->r1 = ir->r0;
	x86_IRs.push_back(ir);
    }
    bb->vec_ir = x86_IRs;
}



void set_last_use(IR *ir, Reg *r, int ic)
{
    if( r && r->last_use < ic + ir->relative )  //relative应付循环体
	r->last_use = ic + ir->relative;
}



std::vector<Reg*> set_use_and_def( Function *func )
{
    std::vector<Reg*> regs;
    int ic = 1;   //instruction counter

    for( int i = 0;  i < func->bbs.size();  i++ ){  
	BB *bb = func->bbs[i];
	//param To Do
	
	for( int i = 0;  i < bb->vec_ir.size();  i++,ic++ ){
	    IR *ir = bb->vec_ir[i];
	    

            if( ir->r0 ){
		if( ir->r0->def == 0 ){
	          ir->r0->def = ic + ir->relative;  //r0第一次出现所在的指令次序
	          if( ir->op == IR_PARA ){
	              ir->r0->rn = num_regs + ir->var->para;//参数的寄存器与其它变量区别开
	          }
		  else {
                      regs.push_back(ir->r0);
		  }
		}
		ir->r0->last_use = ic + ir->relative;   //考虑到无用定义
	    }

	    set_last_use(ir, ir->r1, ic);
	    set_last_use(ir, ir->r2, ic);

	    if( ir->op == IR_CALL ){  //函数调用
	        for( int i = 0;  i < ir->args.size();  i++ ){
	            set_last_use(ir, ir->args[i], ++ic );
		}
	    }

	    if( ir->idx ){  //数组
	        set_last_use( ir, ir->idx, ic );
	    }
	}
    }

    return regs;
}



Reg** create_used_array()
{
    Reg **use = (Reg**)malloc( sizeof(Reg*) * num_regs );
    if( !use )
	error(emEC_Not_enough_memory);

    for( int i = 0;  i < num_regs;  i++ ){
        use[i] = NULL;
    }

    return use;
}



//淘汰last_use最小的寄存器,后面有可能会用到前面的变量
int choose_to_spill( Reg **used )
{
    int k = 0;
    for( int i = 1; i < num_regs; i++ ){
        if( used[k]->last_use > used[i]->last_use )
            k = i;
    }

    return k;
}



//确定r0寄存器的实际寄存器编号 rn
void set_reg_num( std::vector<Reg*> regs )
{
    Reg **used = create_used_array();
    bool found;

    for( int i = 0;  i < regs.size();  i++ ){
        Reg *r = regs[i];
        if( r->vn == 22 || r->vn == 20 ) {
	    Reg *debug = r;
        }

	found = false;
	for( int i = 0;  i < num_regs-1;  i++ ){
	    if( used[i] == NULL  ||  r->def > used[i]->last_use ){ //不冲突的情况
            //r->def只能大于used[i]->last_use,诸如a++一条ir会产生多条汇编代码
	        r->rn = i;
		used[i] = r;
		found = true;
		break;
	    }
	}

	if( found == false ){
	    //最后一个位置专门留给found=false的寄存器
	    used[num_regs - 1] = r;  
	    int k = choose_to_spill(used);

	    r->rn = k;
	    used[k]->rn = num_regs - 1;
	    used[k]->spill = true;   //used[k]是寄存器指针
	    used[k] = r;
	}
    }
}


void spill_load( std::vector<IR*> v,  Reg *r )
{
    if( r && r->spill == true ){
        IR *newir = newIR( IR_LOAD_SPILL );
	newir->r0 = r;    //结果加载到r0
	newir->var = r->var;  //加载需要用到变量名

	if( r->var->type->ty == emTYPE_ARRAY )
	    newir->size = r->var->type->ary_of->size;
	else
	    newir->size = r->var->type->size;
	v.push_back(newir);
    }
}



void spill_store( std::vector<IR*> v, Reg *r )
{
    if( r && r->spill == true ) {
	IR *newir = newIR( IR_STORE_SPILL );
        newir->r1 = r;  //r1结果保存到栈帧
	newir->var = r->var;
	v.push_back(newir);
    }
}



void emit_spill_code( BB *bb )
{
    std::vector<IR*> IRs;
    for( int i = 0;  i < bb->vec_ir.size();  i++ ){
        IR *ir = bb->vec_ir[i];
        spill_load( IRs, ir->r1 );
        spill_load( IRs, ir->r2 );
	IRs.push_back(ir);
	spill_store(IRs, ir->r0);
    }

    bb->vec_ir = IRs;
}


void alloc_regs()
{
    for( int i = 0;  i < prog.gFuncs.size();  i++ ){
        Function *func = prog.gFuncs[i];

	//将SSA转为2地址
	/*
	for( int i = 0;  i < func->bbs.size();  i++ ){
	    BB *bb = func->bbs[i];
	    SSA_to_x86( bb );
	}
	*/

	//分配寄存器,确定变量的实际寄存器编号以及哪些变量被分配到栈帧中
        std::vector<Reg*> regs = set_use_and_def( func ); //regs内的寄存器不重复
        set_reg_num( regs );
	//delete_useless_def( func );

	//增加存储在栈帧的局部变量
	for( int i = 0;  i < regs.size();  i++ ){
	    Reg *r = regs[i];
	    if( r->spill == true ){
	        Var *var = initVar( "spill", 
			pointer_type( int_type() ), true );
		r->var = var;
		func->localVars.push_back(var);
	    }
	}

        for( int i = 0;  i < func->bbs.size();  i++ ){
	    BB *bb = func->bbs[i];
	    emit_spill_code( bb );
	}
    }
}





