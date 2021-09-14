#include "decl.h"
#include "defs.h"
#include "data.h"


static Function *curFunc = NULL;
static std::vector<BB*> curBBS;
static BB *curBB = NULL;
static std::vector<BB*> brkBB;
static std::vector<BB*> continueBB;


static BB* newBB();
static IR* newIR( int op );
static Reg* imm( int val );
static Reg* gen_expr( ASTnode *n );
static void gen_declaration( ASTnode *n );
static void gen_init_decltor_lit( ASTnode *n );
static void gen_stmt( ASTnode *n );
static void gen_compo_stmt( ASTnode *n );
static void gen_blk_itm_lit( ASTnode *n );
static Reg* gen_lval( ASTnode *n, bool take_addr );
static Reg* gen_load( ASTnode *n );

IR* emit( int op, Reg *r0, Reg *r1, Reg *r2 )
{
    IR *ir = newIR(op);
    ir->r0 = r0;
    ir->r1 = r1;
    ir->r2 = r2;

    return ir;
}



BB* createBB()
{
    BB *bb = new BB();
    bb->label = nlabel++;
    return bb;
}


BB* newBB()
{
    BB *bb = createBB();
    curBBS.push_back( bb );

    return bb;
}



static int debug = 0;
IR* newIR( int op )
{
    IR *ir = new IR();
    ir->op = op;
    ir->size = ir->relative = 0;
    ir->idx = ir->r0 = ir->r1 = ir->r2 = NULL;
    ir->debug = debug++;

    (curBB->vec_ir).push_back(ir);

    return ir;
}




Reg* newReg()
{
    Reg *r = new Reg();

    r->vn = nreg++;
    r->rn = r->para = -1;

    r->promoted = NULL;
    r->def = r->last_use = r->spill = 0;
    r->spill = false;

    return r;
}





Reg* imm( int val )
{
    IR *ir = newIR( IR_IMM );
    Reg *r = newReg();

    ir->imm = val;
    ir->r0 = r;
    ir->size = sizeof(int);

    return r;
}


//跳转bb1
IR* jmp( BB *bb )
{
    IR *ir = newIR( IR_JMP );
    ir->bb1 = bb;
}



//根据寄存器r的值，决定跳转bb1还是bb2
IR* br( Reg *r, BB *succ, BB *fail )
{
    IR *ir = newIR( IR_BR );
    ir->r2 = r;
    ir->bb1 = succ;
    ir->bb2 = fail;
    return ir;
}



//对于循环块而言，将循环体和条件判断的顺序交换
void mark_relative_last_use( BB *cond_bb, BB *body_bb )
{
    int cond_ir_num = cond_bb->vec_ir.size();
    int body_ir_num = body_bb->vec_ir.size();

    for( int i = 0;  i < cond_ir_num;  i++ ){
        IR *ir = cond_bb->vec_ir[i];
        ir->relative = body_ir_num;
    }

    for( int i = 0;  i < body_ir_num;  i++ ){
        IR *ir = body_bb->vec_ir[i];
        ir->relative = -cond_ir_num;
    }
}


ASTnode* getIdentFromDecltor( ASTnode *n, Reg **array_off, int *node_type )
{
    *node_type = n->op;

    switch( n->op ){
        case emND_Ident:
	    return n;
	case emND_Decltor:
	    return n->lhs;
	case emND_ARRAY:    //计算数组的偏移地址
	    Reg *reg_off = imm(1);
            *array_off = reg_off;
       	    Type *arr_ty = n->type;
            for( int i = 0;  i < arr_ty->dimens.size() - 1;  i++ ){ // i > 0
	        if( n->op == emND_ARRAY ) {
	          Reg *r = gen_expr( n->rhs );
	          Reg *dimen = imm( arr_ty->dimens[i] );
	          emit( IR_MUL, NULL, r, dimen );    //返回r1
	          emit( IR_ADD, NULL, reg_off, r );
		  n = n->lhs;
		}else{
		  error( emEC_Syntax_error, n->lineno );
		}
	    }
	    emit( IR_MUL, NULL, reg_off, imm(arr_ty->ary_of->size) );
	    return n;
    }
}



Reg* gen_assign( ASTnode *res, ASTnode *expr )
{
    int node_type;
    Reg *array_off;
    ASTnode *ident = NULL;

    ident = getIdentFromDecltor(res, &array_off, &node_type) ;
    Reg *r1 = gen_lval( res, false );
    Reg *r2 = gen_expr(expr);

    IR *ir = emit( IR_STORE, NULL, r1, r2);

    if( node_type == emND_ARRAY ) {
	ir->size = ident->type->ary_of->size;
	ir->idx = array_off;
    }
    else
	ir->size = ident->type->size;


    if( node_type != emTYPE_ARRAY )   //不考虑数组，数组永远内存读写
	if( ir->size != 8 ){          //进行类型缩放
	    IR *ir2 = emit( IR_SCALE, NULL, r1, NULL );
	    ir2->size = ir->size;
	    //ir2->var = ident->var;
	}

    return r2;
}


// r1 = r1 op r2
// 二元操作，返回的是r1
// 去掉多余括号
Reg* gen_binop( int op, ASTnode *n )
{
    Reg *r0 = NULL, *r1 = NULL, *r2 = NULL;
    if( n->lhs->op != emND_LParen && n->lhs->op != emND_RParen )
        r1 = gen_expr(n->lhs);
    if( n->rhs->op != emND_LParen && n->rhs->op != emND_RParen )
        r2 = gen_expr(n->rhs);
    emit( op, r0, r1, r2);
    return r1;
}


Reg* gen_unary_expr( ASTnode *n )
{
    int op = n->lhs->op;
    ASTnode *lval = n->rhs;
    switch( op ){
        case emND_Inc: {
            Reg *dst = newReg();
	    Reg *addr = gen_lval(lval, false);
            emit( IR_LOAD, dst, NULL, addr )->size = lval->type->size;

            IR  *ir = emit(IR_INC, NULL, dst, NULL );
            ir->imm = 1;    //加一自增
	    emit( IR_STORE, NULL, addr, dst )->size 
		    = lval->type->size;
	    return dst;
        }
        case emND_Dec: {
            Reg *dst = newReg();
	    Reg *addr = gen_lval(lval, false);
            emit( IR_LOAD, dst, NULL, addr )->size = lval->type->size;

            IR  *ir = emit(IR_DEC, NULL, dst, NULL );
	    ir->imm = 1;
	    emit( IR_STORE, NULL, addr, dst )->size 
		    = lval->type->size;
	    return dst;
        }
        case emND_Unary_Plus: {
	    return gen_expr( lval );
        }
        case emND_Unary_Minus: {
	    return emit( IR_Unary_Minus, gen_load(lval), NULL, NULL)->r0;
	    break;
        }
	case emND_ADDR: {
	    return gen_lval( lval, true );
	    break;
	}
    }
}



void argument_expression_list( IR *ir, ASTnode *n )
{
    if( n->op == emND_ArgExpr ){
        argument_expression_list(ir, n->lhs);
        argument_expression_list(ir, n->rhs);
    }
    else if( n->op != emND_RParen &&  n->op != emND_LParen ){
        ir->args.push_back( gen_expr( n ) );
    }
}



Reg* gen_post_expr( ASTnode *n )
{
    switch( n->op ){
        case emND_Call:{   
	    IR *ir = new IR();
	    if( n->rhs )  //最多6个参数
                argument_expression_list( ir, n->rhs );
	    ir->op = IR_CALL;
	    ir->r0 = newReg();   //r0返回
	    ir->name = n->lhs->name;
            (curBB->vec_ir).push_back(ir);

	    bool has_def = false;
	    for( int i = 0;  i < prog.gFuncs.size();  i++ ){
	      if( prog.gFuncs[i]->name == ir->name )
		  has_def = true;
	    }
            if( buil_in_funcs.find(ir->name) != buil_in_funcs.end() )
	      has_def = true;
	    if( has_def == false )
	      Perror( emEC_Undeclared_identifier, n->lineno, ir->name.data() );
            

	    return ir->r0;	       
        }
        case emND_PostExpr: {
	    ASTnode *post_expr = n->lhs;
            Reg *dst = newReg();
	    Reg *addr = gen_lval(post_expr, false );
            emit( IR_LOAD, dst, NULL, addr )->size = post_expr->type->size;
	    switch( n->rhs->op ){
	        case emND_Inc:{
                    Reg *ret = emit( IR_MOV, newReg(), NULL, dst )->r0;
                    IR  *ir = emit(IR_INC, NULL, dst, NULL );
		    ir->imm = 1;
	            emit( IR_STORE, NULL, addr, ir->r1 )->size = post_expr->type->size;
		    return ret;
                }
	        case emND_Dec:{
		    IR *ir2 = emit( IR_MOV, newReg(), NULL, dst );  //24
                    Reg *ret = ir2->r0;
                    IR  *ir = emit(IR_DEC, NULL, dst, NULL );
		    ir->imm = 1;
	            emit( IR_STORE, NULL, addr, ir->r1 )->size = post_expr->type->size;
		    return ret;
                }
	    }
	}
	default: 
	    assert( 0 && "postfix expression type wrong" );
    }
}


Reg* gen_expr( ASTnode *n )
{
    switch( n->op ){
        case emND_Const:
	    return imm( n->val );
	case emND_Assign: 
	    return gen_assign( n->lhs, n->rhs );
	case emND_Eq:  // ==
            return gen_binop( IR_EQ, n );
	case emND_Ne:  // !=
            return gen_binop( IR_NE, n );
	case emND_Lt:
	    return gen_binop( IR_LT, n );
	case emND_Le:
	    return gen_binop( IR_LE, n );
	case emND_Gt:
	    return gen_binop( IR_GT, n );
	case emND_Ge:
	    return gen_binop( IR_GE, n );
	case emND_Plus:
	    return gen_binop( IR_ADD, n );
	case emND_Minus:
	    return gen_binop( IR_SUB, n );
	case emND_Mul:
	    return gen_binop( IR_MUL, n );
	case emND_Div:
	    return gen_binop( IR_DIV, n );
	case emND_Mod:
	    return gen_binop( IR_MOD, n );
	case emND_UnaryExpr:
	    return gen_unary_expr( n );
	case emND_Ident: {    //右值加载
	    if( n->var->para == -1 ) {
	      return gen_load( n );
	    }
	    else {   //变量为参数时
              IR *ir = newIR(IR_PARA);
              ir->r0 = newReg();
              ir->r0->para = n->var->para;
              ir->var = n->var;  //通过绑定var，将所有表示参数的寄存器连接
              ir->var->take_addr = true;
	      if( n->var->type->ty == emTYPE_ARRAY )
		  ir->size = n->var->type->ary_of->size;
	      else
		  ir->size = n->var->type->size;
	      return ir->r0;
	    }
	}
	case emND_PostExpr:
	    return gen_post_expr( n ); 
	case emND_Call:
	    return gen_post_expr( n ); 
	case emND_ARRAY:
            return gen_load( n );
	default: 
	    assert( 0 && "Unknown AST type" );
    }
}


//获得地址
Reg* gen_lval( ASTnode *n, bool take_addr )
{
    IR *ir = NULL;

    if( n->type == NULL ){
	Perror( emEC_Undeclared_identifier, n->lineno, n->name.data() );
    }

    if( n->op == emND_Ident ){
        if( n->type->ty != emND_StructSpec ){
	    if( n->var->is_local ){
                if( n->var->para == -1 ){
                    ir = newIR(IR_BPREL);   //bp
	            ir->r0 = newReg();
		    ir->var = n->var;    //offset
		    if( !ir->var->take_addr )
                      ir->var->take_addr = take_addr;
		}
		else{
		    ir = newIR(IR_PARA);
		    ir->r0 = newReg();
		    ir->r0->para = n->var->para;
		    ir->var = n->var;
		    ir->var->take_addr = true;
	            if( n->var->type->ty == emTYPE_ARRAY )
		        ir->size = n->var->type->ary_of->size;
	            else
		        ir->size = n->var->type->size;
	        }
	    }else{
	        ir = newIR( IR_LABEL_ADDR );  //全局变量
		ir->var = n->var;
		ir->r0 = newReg();
		ir->name = n->name;
	    }

	    return ir->r0;
	}
    }
    else if( n->op == emND_ARRAY ){
	Reg *reg_off = imm(1);
	Type *arr_ty = n->type;
        for( int i = 0;  i < arr_ty->dimens.size() - 1;  i++ ){
	    if( n->op == emND_ARRAY ) {
	      Reg *r = gen_expr( n->rhs );
	      Reg *dimen = imm( arr_ty->dimens[i] );
	      emit( IR_MUL, NULL, r, dimen );    //返回r1
	      emit( IR_ADD, NULL, reg_off, r );
	      n = n->lhs;
	    }else{
	      error( emEC_Syntax_error, n->lineno );
	    }
        }
	emit( IR_MUL, NULL, reg_off, imm(arr_ty->ary_of->size) );
		
	if( n->var->is_local ){
            if( n->var->para == -1 ){
                ir = newIR(IR_BPREL);   //bp
                ir->r0 = newReg();
		ir->idx = reg_off;
                ir->var = n->var;    //offset
		if( !ir->var->take_addr )
                  ir->var->take_addr = take_addr;
	    }
	    else{
	        ir = newIR(IR_PARA);
	        ir->r0 = newReg();
	        ir->r0->para = n->var->para;
	        ir->var = n->var;
		ir->var->take_addr = true;
		ir->idx = reg_off;
	        if( n->var->type->ty == emTYPE_ARRAY )
		    ir->size = n->var->type->ary_of->size;
	        else
		    ir->size = n->var->type->size;
	    }
	}else{
	    ir = newIR( IR_LABEL_ADDR );  //全局变量
            ir->r0 = newReg();
            ir->idx = reg_off;
            ir->var = n->var;    
	    ir->name = n->name;
	}
    }
    else
	Perror( emEC_Left_operand_must_be_l_value, n->lineno );


    return ir->r0;
}



Reg* gen_load( ASTnode *n )
{
    Reg *src = gen_lval(n, false);

    Reg *dst = newReg();
    IR *ir = emit( IR_LOAD, dst, NULL, src );
    if( n->op == emND_ARRAY )
      ir->size = n->type->ary_of->size;
    else 
      ir->size = n->type->size;
    
    return dst;
}


void gen_declaration( ASTnode *n )
{
    ASTnode *init_decltor_lit = n->rhs;
    gen_init_decltor_lit(n->rhs);
}




void gen_init_decltor_lit( ASTnode *n )
{
    if( n->op == emND_GLUE ){
        gen_init_decltor_lit(n->lhs);
        gen_init_decltor_lit(n->rhs);
    }
    else if( n->op == emND_InitDecltor ){
        if( n->rhs ){    //只翻译赋值语句
	    ASTnode *decltor = n->lhs;

	    Reg *r1 = gen_lval( decltor, false );
	    Reg *r2 = gen_expr( n->rhs );
            IR *ir = emit(IR_STORE, NULL, r1, r2);
            ir->size = decltor->type->size;
	}
    }
}



//statement
//	: compound_statement     { $$ = linkASTnode(emND_Stmt, $1, NULL); }
//	| expression_statement   { $$ = linkASTnode(emND_Stmt, $1, NULL); }
//	| selection_statement    { $$ = linkASTnode(emND_Stmt, $1, NULL); }
//	| iteration_statement    { $$ = linkASTnode(emND_Stmt, $1, NULL); }
//	| jump_statement         { $$ = linkASTnode(emND_Stmt, $1, NULL); }
void gen_stmt( ASTnode *n )
{
    if( !n )
	return;

    switch( n->op ){
        case emND_Ret: {
	    if(curFunc->retType->ty == emTYPE_VOID && n->lhs ){
	        Perror(emEC_Void_function_returning_a_value, n->lineno, 
		   curFunc->name.data() );
		return;
	    }
            curBB = newBB();   //新的basic block
	    Reg *r2 = gen_expr(n->lhs);
	    emit(IR_RETURN, NULL, NULL, r2);  //2条指令的r2一致
	    break;
	}
	case emND_ExprStmt: {
	    gen_expr( n->lhs );
	    break;
        }
	case emND_While: {
            curBB = newBB(); 
	    BB *cond_bb = curBB;

	    ASTnode *cond_ast = n->lhs;
	    ASTnode *body_ast = n->rhs;
	    BB *body_bb = createBB();
	    BB *br_bb = createBB();

	    brkBB.push_back(br_bb);    //记录label，break与continue需要使用
	    continueBB.push_back(cond_bb);

	    Reg *cond_reg = gen_expr( cond_ast );  //条件判断
            br( cond_reg, body_bb, br_bb );
            for( int i =0;  i < curBB->vec_ir.size();  i++ ){
	        if( curBB->vec_ir[i]->var )
	            curBB->vec_ir[i]->var->take_addr = true;
	    }

	    curBB = body_bb;        //块
            curBBS.push_back( body_bb );
            gen_stmt( body_ast->lhs );
	    jmp( cond_bb );     //跳回判断语句

            mark_relative_last_use( cond_bb, body_bb );

	    curBB = br_bb;
            curBBS.push_back( br_bb );

	    brkBB.pop_back();
	    continueBB.pop_back();
	    break;		 
	}
	case emND_If: {
	    ASTnode *cond_ast = n->lhs;  //3者顺序固定
	    ASTnode *body_ast = n->stmt;
	    ASTnode *else_ast = n->rhs;

	    curBB = newBB();
            BB *cond_bb = curBB;
	    BB *body_bb = createBB();
	    BB *br_bb = createBB();
	    BB *else_bb = NULL;
	    if( else_ast )
	        else_bb = createBB();
            
	    //条件判断
            Reg *cond_reg = gen_expr( cond_ast );
	    if( else_ast )
	        br( cond_reg, body_bb, else_bb );
	    else
		br( cond_reg, body_bb, br_bb );

            curBB = body_bb;
	    curBBS.push_back( curBB );
            gen_stmt( body_ast->lhs );
	    jmp( br_bb );

	    if( else_ast ){
                curBB = else_bb;
		curBBS.push_back( curBB );
		gen_stmt( else_ast->lhs );
		jmp(br_bb);
            }

	    curBB = br_bb;
	    curBBS.push_back( curBB );

	    break;
	}
        case emND_For: {
	    ASTnode *ast_init = n->lhs;
	    ASTnode *ast_cond = n->mid;
	    ASTnode *ast_inc = n->rhs;
            ASTnode *ast_stmt = n->stmt;
            
	    if( ast_init ){
		if( ast_init->op == emND_ExprStmt )
    	          gen_expr( ast_init->lhs );
	        else if( ast_init->op == emND_Decltion )
		  gen_declaration( ast_init );
	    }

	    curBB = newBB();
            BB *cond_bb = curBB;
	    BB *body_bb = createBB();
	    BB *br_bb = createBB();

	    Reg *cond_reg = NULL;   //条件判断
	    if( ast_cond )
	        cond_reg = gen_expr( ast_cond->lhs );
	    else{
	        cond_reg = imm(1);
	    }
	    br( cond_reg, body_bb, br_bb );

            //条件判断中的变量使用堆栈读写
            for( int i =0;  i < curBB->vec_ir.size();  i++ ){
	        if( curBB->vec_ir[i]->var )
	            curBB->vec_ir[i]->var->take_addr = true;
	    }

	    curBB = body_bb;           //循环体
	    curBBS.push_back( curBB );
	    gen_stmt( ast_stmt->lhs );
            if( ast_inc )
		gen_expr( ast_inc );
            jmp( cond_bb );

            mark_relative_last_use( cond_bb, body_bb );

	    curBB = br_bb;
	    curBBS.push_back( curBB );

	    break;	       
        }
	case emND_CompoStmt:
	    gen_compo_stmt(n);
	    break;
	case emND_Brk: {
            if( brkBB.size() == 0 ) {
		Perror(emEC_Illegal_break, n->lineno);
		return;
	    }
	    jmp( brkBB.back() );
	    break;
        }
	case emND_Ctn: {
            if( continueBB.size() == 0 ){
		Perror(emEC_Illegal_continue, n->lineno);
		return;
	    }
	    jmp( continueBB.back() );
	    break;
	}
        default : 
	    error(emEC_Syntax_error, n->lineno);
    }
}


void gen_compo_stmt( ASTnode *n )
{
    ASTnode *blk_itm_lit = n->lhs;
    gen_blk_itm_lit( blk_itm_lit );
}


void gen_blk_itm_lit( ASTnode *n )
{
    if( n->op == emND_GLUE ){
        gen_blk_itm_lit(n->lhs);
	gen_blk_itm_lit(n->rhs);
    }
    else if( n->op == emND_Decltion ){
        gen_declaration(n);
    }
    else if( n->op == emND_Stmt ){
        gen_stmt(n->lhs);
    }
} 

void gen_ir()
{
    if( !lookforFunc( "main" ) ){
		Perror(emEC_No_main_function, yylineno);
		return;
    }
    for( int i = 0;  i < prog.gFuncs.size();  i++ ){
        curFunc = prog.gFuncs[i];
		curBB = newBB();   //解决基本块Basic block，花括号处理 
		if( curFunc->body )
            gen_compo_stmt( curFunc->body );
		curFunc->bbs = curBBS;
		curBBS.clear();
    }
}

