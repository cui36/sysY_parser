#include "decl.h"
#include "defs.h"
#include "data.h"



static std::vector<Stack> env;       //作用域
static std::vector<Var*> locVars;    //函数的局部变量
static bool is_loclIdent;            //标识当前变量是全局亦或局部


static void addLocalVar( Var *locVar );
static void addGlobVar( Var *gVar );

static Function* initFunc( std::string name, Type *retType, ASTnode *body );

static Type* type_specifier( ASTnode *n );

//declaration
static ASTnode* declarator( Type *ty, ASTnode *n );
static void init_declarator_list( Type *ty, ASTnode *n );
static void declaration( ASTnode *n );

//function definition
static void function_definition( ASTnode *n );
static ASTnode* compound_statement( ASTnode *n );
static ASTnode* block_item_list( ASTnode *n );
static void expression_statement( ASTnode *n );



void parseASTTree( ASTnode *n )
{
    if( n->op == emND_GLUE ) {   //胶水节点，解析左右
        parseASTTree( n->lhs );
        parseASTTree( n->rhs );
    }
    else if( n->op == emND_Decltion ){ //变量声明
        is_loclIdent = false;  //如果是变量，则是全局声明
        declaration(n);
    }
    else if( n->op == emND_Func ) {
        is_loclIdent = true;
        function_definition(n);
    }
}



Function* lookforFunc( std::string name )
{
    //function
    if( buil_in_funcs.find(name) != buil_in_funcs.end() )
	return buil_in_funcs[name];

    for( int i = 0;  i < prog.gFuncs.size();  i++ ){
        if( prog.gFuncs[i]->name == name )
	    return prog.gFuncs[i];
    }

    return NULL;
}



Var* lookforVar( std::string name, int lineno )
{
    for( int i = env.size() - 1;  i >= 0;  i-- ){  //局部变量
        Stack stack = env[i];
	if( stack.vars.find(name) != stack.vars.end() ){
	    return stack.vars[name];
	}
    }
    
    for( int i = 0;  i < prog.gVars.size();  i++ ){  //全局变量
        if( prog.gVars[i]->name == name )
	    return prog.gVars[i];
    }

    //该变量定义不存在
    error(emEC_Undeclared_identifier, lineno, name.data() );
}



Var* initVar( std::string name, Type *ty, bool is_local)
{
    Var *pV = new Var();

    pV->name = name;
    pV->type = ty;
    pV->is_local = is_local;

    pV->globVal = pV->off = 0;
    pV->para = -1;

    pV->promoted = NULL;

    pV->take_addr = false;  //优化

    return pV;
}


void addGlobVar( Var *gVar )
{
    prog.gVars.push_back(gVar);
}


void addLocalVar( Var *locVar )
{
    env.back().vars.insert( {locVar->name, locVar} );
    locVars.push_back(locVar);
}


void addFunc( Function *func )
{
    func->localVars = locVars;
    locVars.clear();
    prog.gFuncs.push_back(func);
}



Function* initFunc( std::string name, Type *retType, ASTnode *body, 
		std::vector<ASTnode*> v_para )
{
    Function *func = new Function();

    func->name = name;
    func->retType = retType;
    func->body = body;
    func->para = v_para;

    return func;
}


void enterBlock()
{
    Stack s;
    env.push_back(s);
}


void exitBlock()
{
    env.pop_back();
}

/********************  function definition  ************************/

/*
 * int fn( int a ) {}
 * */
static int para_idx = 0;
void parameter_list( ASTnode *n, std::vector<ASTnode*> &v_para )
{
    if( !n )
	return;

    switch( n->op ) {
	case emND_Para : {
            parameter_list( n->lhs, v_para );
            parameter_list( n->rhs, v_para );
	    break;
	}
	case emND_Decltor: {
            ASTnode *ty_node = n->lhs;
	    ASTnode *para_node = n->rhs;

	    Type *ty = type_specifier( ty_node );
            para_node = declarator(ty, para_node);
	    v_para.push_back( para_node );

	    para_node->var->para = para_idx++;  //记录下是函数的第几个参数
            break;
	}
        default: assert( 0 && "wrong type of parameter" );
    }
}



//function_definition : type_specifier declarator compound_statement   
void function_definition( ASTnode *n )
{
    ASTnode *ty_spec = n->lhs;
    ASTnode *decltor = n->mid;
    ASTnode *compo_stmt = n->rhs;

    Type *ty = type_specifier( ty_spec );

    std::string funcName;    // func() or fun( para )
    if( decltor->op == emND_Func  &&  decltor->lhs->op == emND_Ident ){
        funcName = decltor->lhs->name;
    }
    else{
	Perror( emEC_Syntax_error, n->lineno );
	return;
    }

    if( lookforFunc( funcName ) )
        Perror( emEC_Function_redefined, decltor->lineno, funcName.data() );

    ASTnode *para = decltor->rhs;   //参数
    std::vector<ASTnode*> v_para;
    para_idx = 0;
    if( para && para->op == emND_Para ){   //参数
        enterBlock();
        parameter_list( para, v_para );
    }


    //compound_statement
    if( compo_stmt ){
        compound_statement( compo_stmt );
    }
    
    if( para_idx != 0 )
        exitBlock();   //退出形参作用域


    Function *func = initFunc( funcName, ty, compo_stmt, v_para );
    addFunc( func );
}



//compound_statement
//	: '{' '}'    { $$ = NULL; }
//	| '{' block_item_list '}'    { $$ = compoASTnode($2); }
ASTnode* compound_statement( ASTnode *n )
{
    assert( n->op == emND_CompoStmt );
    enterBlock();
    ASTnode *blk_itm_lit = block_item_list(n->lhs);
    exitBlock();
    return blk_itm_lit;
}



void argument_expression_list( ASTnode *n, int *argNum )
{
    if( n->op == emND_ArgExpr ){
        argument_expression_list( n->lhs, argNum );
	argument_expression_list( n->rhs, argNum );
    }
    else{
        expression_statement(n);
	(*argNum)++;
    }
}



//确定算式中变量的类型
void expression_statement( ASTnode *n )
{
    if( !n )
	return ;

    switch( n->op ){
        case emND_Assign: {
	    ASTnode *res = n->lhs;
	    ASTnode *expr = n->rhs;
	    ASTnode *arry_ast = res;

	    int dimens = 0;
	    while( res->op == emND_ARRAY ){
                expression_statement(res->rhs);
		dimens++;
	        res = res->lhs;
	    }

	    Var *var = lookforVar( res->name, res->lineno );
            if( res->op ==emND_ARRAY && var->type->dimens.size() != dimens + 1){
	        Perror("on Line %d : assignment to expression with array type",
			res->lineno);
		return;
	    }

	    arry_ast->type = res->type = var->type;  //数组的第一个节点
	    arry_ast->var = res->var = var;

            expression_statement(expr);
	    break;
	}
	case emND_Ge:
	case emND_Gt:
	case emND_Le:
	case emND_Lt:
	case emND_Eq:
	case emND_Ne:
	case emND_Mod:
	case emND_Mul:
	case emND_Div:
	case emND_Minus:
	case emND_Plus: {
            expression_statement(n->lhs);
            expression_statement(n->rhs);
	    break;
	}
	case emND_Ident: {
	    Var *var = lookforVar( n->name, n->lineno );
	    n->type = var->type;
	    n->var = var;
	    break;
	}
	case emND_Call: {
	    Function *func = lookforFunc(n->lhs->name);
	    if( !func ){
                Perror(emEC_Undeclared_identifier, n->lhs->lineno, n->lhs->name.data() );
	    }
	    if( n->rhs ){  //有参数，确定数据类型
		int argNum = 0;
	        argument_expression_list( n->rhs, &argNum );
		if( argNum > func->para.size() ){
		    Perror(emEC_Too_many_argument, n->lhs->lineno, n->lhs->name.data());
		    return;
		}else if( argNum < func->para.size() ){
		    Perror(emEC_Too_few_argumnet, n->lhs->lineno, n->lhs->name.data());
		    return;
		}
	    }
	    break;
	}
	case emND_UnaryExpr: {
	    expression_statement( n->rhs );
	    if( n->rhs->type == NULL || n->rhs->type->ty == emTYPE_STRING ){
	        Perror(emEC_Left_operand_must_be_l_value, n->rhs->lineno );
		return;
	    }
	    break;
	}
	case emND_PostExpr: {
	    expression_statement( n->lhs );
	    if( n->lhs->type == NULL || n->lhs->type->ty == emTYPE_STRING ){
	        Perror(emEC_Left_operand_must_be_l_value, n->lhs->lineno );
		return;
	    }
	    break;		    
	}
	case emND_ARRAY: {
	    ASTnode *ident = n;
	    ASTnode *array_ast = n;
	    while( ident->op == emND_ARRAY ) {
		expression_statement( ident->rhs );
                ident = ident->lhs;
	    }
	    Var *var = lookforVar( ident->name, ident->lineno );
	    if( var->type->ty != emTYPE_ARRAY ){
	        Perror(emEC_Subscript_value_is_not_array, ident->lineno);
		return;
	    }
	    ident->type = array_ast->type = var->type;
	    ident->var = array_ast->var = var;
	    break;
	}
	case emND_String:
	case emND_Const:
	    break;
    }

}



//statement
//	: compound_statement     { $$ = mkunaryASTnode(emND_Stmt, $1); }
//	| expression_statement   { $$ = mkunaryASTnode(emND_Stmt, $1); }
//	| selection_statement    { $$ = mkunaryASTnode(emND_Stmt, $1); }
//	| iteration_statement    { $$ = mkunaryASTnode(emND_Stmt, $1); }
//	| jump_statement         { $$ = mkunaryASTnode(emND_Stmt, $1); }
//
//jump_statement
//	: CONTINUE ';'  { $$ = ctnASTnode(); }
//	| BREAK ';'     { $$ = brkASTnode(); }
//	| RETURN ';'    { $$ = retASTnode(NULL); }
//	| RETURN assignment_expression ';' { $$ = retASTnode($2); }
void statement( ASTnode *n )
{
    if( !n )
	return;

    switch( n->op ){
        case emND_Ret:
	    if( n->lhs ){
	        expression_statement( n->lhs );
	    }
	    break;
	case emND_ExprStmt:
	    expression_statement(n->lhs);
	    break;
	case emND_While: {
	    expression_statement( n->lhs );
	    statement( n->rhs );
	    break;		 
	}
	case emND_For: {
	    bool has_declaration = false;
	    if( n->lhs ){
	        if( n->lhs->op == emND_ExprStmt )
	          expression_statement( n->lhs->lhs );
		else if( n->lhs->op == emND_Decltion ) {
		  has_declaration = true;
		  enterBlock();     // for( ; ; )  一个作用域
		  declaration(n->lhs);
		}
	    }
	    if( n->mid && n->mid->op == emND_ExprStmt )
		expression_statement( n->mid->lhs );
	    if( n->rhs )
		expression_statement( n->rhs );
	    if( n->stmt && n->stmt->op == emND_Stmt )
		statement(n->stmt->lhs);
	    if( has_declaration )
		exitBlock();
	    break;
        }
	case emND_Stmt:
	    statement( n->lhs );
	    break;
	case emND_CompoStmt: {
	    ASTnode *blk_itm_lit = n->lhs;
	    block_item_list( blk_itm_lit );
	    break;
	}
	case emND_If: {
	    expression_statement( n->lhs );
	    statement( n->rhs );
	    statement( n->stmt );
	    break;	      
	}
    }

}



//block_item_list
//	: block_item   { $$  = $1; }
//	| block_item_list block_item   { $$ = glueASTnode( $1, $2 ); }
ASTnode* block_item_list( ASTnode *n )
{
    if( n->op == emND_GLUE ){
        block_item_list( n->lhs );
	block_item_list( n->rhs );
    }
    else if( n->op == emND_Decltion ){
        declaration(n);
    }
    else if( n->op == emND_Stmt ){
        statement( n->lhs );
    }

    return n;
}



/**********************  declaration  **********************/




//declaration: type_specifier init_declarator_list ';'   
void declaration( ASTnode *n )
{
    Type *ty = type_specifier(n->lhs);
    if( ty->ty == emTYPE_VOID ){
        Perror( emEC_Void_cannot_be_an_argument_type, n->lineno );
	return;
    }
    ASTnode *init_decltor_lit = n->rhs;
    if ( init_decltor_lit ){
        init_declarator_list( ty, init_decltor_lit );
    }

}


//init_declarator_list
//   : init_declarator   { $$ = $1; }
//   | init_declarator_list ',' init_declarator  { $$ = glueASTnode( $1, $3); }
//
//init_declarator
//	: declarator  { $$ = mkunaryASTnode( emND_InitDecltor, $1); }
//      | declarator '=' assignment_expression { $$ = mkbinaASTnode( emND_InitDecltor, $1, $3); }
void init_declarator_list( Type *ty, ASTnode *n )
{
    if( n ){
        if( n->op == emND_GLUE ){
	    init_declarator_list(ty, n->lhs);
	    init_declarator_list(ty, n->rhs);
	}
	else if( n->op == emND_InitDecltor ){
            ASTnode *decltor = declarator( ty, n->lhs );
            expression_statement( n->rhs );

	    if( is_loclIdent == false && n->rhs ) {  //全局赋值
	      if( ty->ty == emTYPE_STRING ) {        //字符串
		  if( n->rhs->op != emND_String_Literal ){
		    Perror(emEC_Invalid_Initializer, n->rhs->lineno);
		    return;
		  }
		  else {
		    decltor->var->globStr = n->rhs->str;
		    int len = n->rhs->str.length();
		    int align = 1;
		    if( len >= 32 )
			align = 32;
		    else if( len >= 16 )
			align = 16;
		    else if( len >= 8 )
			align = 8;
		    ty->len = len + 1;
		    ty->align = align;
		    ty->size = 8;
		  }
	      }
	      else if( n->rhs->op == emND_Const ){   //整型数值
	          decltor->var->globVal = n->rhs->val;
	      }
	      else{
	          Perror( "Syntax error on line %d : Global variables only "
				  "support constant definitions", n->rhs->lineno );
		  return;
	      }
	    }
	    else if( ty->ty == emND_String ){   //字符串只能是全局
	      Perror( "Syntax error on line %d : "
		 "Partially defined strings are not supported.", n->lineno );
   	      return;
	    }

	}

    }

}


// a, (a+b), a[2][3], b[], func(), func(int), func(int a)
//declarator
//	: IDENTIFIER  { $$ = $1; } 
//	| declarator '[' assignment_expression ']'
//             {$$ = mkbinaASTnode( emND_ARRAY, $1, $3 ); }
ASTnode* declarator( Type *ty, ASTnode *n )
{
    ASTnode *node = n;
    Var *newVar = NULL;
    int len = 1;

    switch( n->op ){
	case emND_Decltor :
            return declarator( ty, n->lhs );
        case emND_ARRAY :{    //数组, 数组维数只支持整数
	    node->type = ty;    //第一个节点同样绑定类型
	    std::vector<int> dimens;
	    dimens.push_back(1);
            while( node->op == emND_ARRAY ){
	        if( node->rhs->op != emND_Const ){
	            error( emEC_Expected_constant_expression ,n->lineno );
	        }
		dimens.push_back( node->rhs->val * dimens.back() );
		len *= node->rhs->val;
                node = node->lhs;
	    }
    
	    std::string name = node->name;
            Type *array_ty = array_type( ty, len );
	    array_ty->dimens = dimens;
    
            newVar = initVar( name, array_ty, is_loclIdent );
	    node->type = array_ty;
	    node->var = newVar;
            break;
        }
	case emND_Ident :{   //IDENTIFIER
            std::string identName = node->name;
    
            newVar = initVar( identName, ty, is_loclIdent );
	    node->type = ty;   //节点记录下类型
            node->var = newVar;
	    break;
        }
	case emND_Func:
            error("Line %d :  function ‘%s’ is initialized like a variable",
	        n->lineno, n->lhs->name.data() );   //exit(0)

    }


    if( is_loclIdent == false ){
        for( int i = 0;  i < prog.gVars.size();  i++ ){  //全局变量
            if( prog.gVars[i]->name == node->name ){
	      error(emEC_Redefinition, node->lineno, node->name.data());
	    }
        }
	addGlobVar( newVar ); //记录下全局变量

    }else {
        Stack stack = env.back();
	if( stack.vars.find(node->name) != stack.vars.end() ){
	    error(emEC_Redefinition, node->lineno, node->name.data());
	}
        addLocalVar( newVar ); //记录局部变量
    }

    
    return node;
}




Type* type_specifier( ASTnode *n )
{
    switch( n->op ){
        case emND_VOID: 
	     return void_type();
        case emND_CHAR: 
	     return char_type();
        case emND_SHORT: 
	     return short_type();
        case emND_INT: 
	     return int_type();
	case emND_String:
	     return string_type();
        case emND_StructSpec: 
	         // to do struct
        default: 
             error(emEC_Syntax_error, n->lineno);
    }
}
