%locations
%error-verbose

%{
#include "decl.h"
#include "defs.h"
#include "data.h"



static void yyerror( const char *format, ... );

static ASTnode* initASTNode();
static ASTnode* mkzeroASTnode( int op );
static ASTnode* mkTriASTnode( int op, ASTnode *lhs, ASTnode *mid, ASTnode *rhs );
static ASTnode* mkbinaASTnode( int op, ASTnode *lhs, ASTnode *rhs );
static ASTnode* mkunaryASTnode( int op, ASTnode *lhs );
static ASTnode* mkquatASTnode( int op, ASTnode *lhs, ASTnode *mid, 
                                 ASTnode *rhs, ASTnode *stmt );

static ASTnode* glueASTnode( ASTnode* n1, ASTnode *n2 );

%}



%union{
    struct _ASTnode *astNode;
}


%token <astNode> CONSTANT STRING_LITERAL IDENTIFIER
%token <astNode> CHAR SHORT INT BOOL VOID STRING ADDR
%token <astNode> INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token <astNode> AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token <astNode> SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token <astNode> XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token <astNode> TYPEDEF EXTERN STATIC AUTO REGISTER INLINE RESTRICT
%token <astNode> LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE
%token <astNode> COMPLEX IMAGINARY
%token <astNode> STRUCT ENUM ELLIPSIS

%token <astNode> CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN


%type <astNode> function_definition  type_specifier compound_statement
%type <astNode> declaration translation_unit external_declaration
%type <astNode> jump_statement block_item_list assignment_operator
%type <astNode> expression_statement statement iteration_statement 
%type <astNode> selection_statement block_item init_declarator_list
%type <astNode> logical_and_expression init_declarator 
%type <astNode> logical_or_expression struct_specifier struct_declaration_list 
%type <astNode> equality_expression relational_expression struct_declaration
%type <astNode> additive_expression multiplicative_expression
%type <astNode> unary_operator unary_expression postfix_expression
%type <astNode> argument_expression_list assignment_expression
%type <astNode> primary_expression declarator
%type <astNode> declarator_list parameter_list parameter_declaration
%type <astNode> identifier_list
%type <astNode> '*' '+' '-' '&'

%start program_start



%%

primary_expression
	: IDENTIFIER  { $$ = $1; } 
	| CONSTANT    { $$ = $1; }
	| STRING_LITERAL  { $$ = $1; } 
	| '(' assignment_expression ')'  { $$ = $2; }
	;

postfix_expression
	: primary_expression
              { $$ = $1; }
	| postfix_expression '[' assignment_expression ']'  
              { $$ = mkbinaASTnode(emND_ARRAY, $1, $3 ); }
	| postfix_expression '(' ')'
              { $$ = mkunaryASTnode(emND_Call, $1 ); }
	| postfix_expression '(' argument_expression_list ')'
              { $$ = mkbinaASTnode(emND_Call, $1, $3 ); }
	| postfix_expression '.' IDENTIFIER
              { //$$ = linkASTnode(emND_PostExpr, $1, $3, NULL);
              }
	| postfix_expression INC_OP
              { $$ = mkbinaASTnode(emND_PostExpr, $1, mkzeroASTnode(emND_Inc)); }
	| postfix_expression DEC_OP
              { $$ = mkbinaASTnode(emND_PostExpr, $1, mkzeroASTnode(emND_Dec)); }
	;

argument_expression_list
	: assignment_expression   { $$ = $1; }
	| argument_expression_list ',' assignment_expression  
              { $$ = mkbinaASTnode( emND_ArgExpr, $1, $3); }
	;

unary_expression
	: postfix_expression
               { $$ = $1; }
	| INC_OP unary_expression  
               { $$ =mkbinaASTnode(emND_UnaryExpr, mkzeroASTnode(emND_Inc), $2); }
	| DEC_OP unary_expression
               { $$ =mkbinaASTnode(emND_UnaryExpr, mkzeroASTnode(emND_Dec), $2); }
        | unary_operator unary_expression
               { $$ = mkbinaASTnode(emND_UnaryExpr, $1, $2); }
	;

unary_operator
	: '+'  { $$ = mkzeroASTnode( emND_Unary_Plus ); }
	| '-'  { $$ = mkzeroASTnode( emND_Unary_Minus ); }
        | '&'  { $$ = mkzeroASTnode( emND_ADDR); }
	;

multiplicative_expression
	: unary_expression { $$ = $1; }
	| multiplicative_expression '*' unary_expression
             { $$ =mkbinaASTnode(emND_Mul, $1, $3 ); }
	| multiplicative_expression '/' unary_expression
             { $$ =mkbinaASTnode(emND_Div, $1, $3 ); }
	| multiplicative_expression '%' unary_expression
             { $$ =mkbinaASTnode(emND_Mod, $1, $3 ); }
	;

additive_expression
	: multiplicative_expression{ $$ = $1; }
	| additive_expression '+' multiplicative_expression
             { $$ =mkbinaASTnode(emND_Plus, $1, $3 ); }
	| additive_expression '-' multiplicative_expression
             { $$ =mkbinaASTnode(emND_Minus, $1, $3 ); }
	;

relational_expression
	: additive_expression{ $$ = $1; }
	| relational_expression '<'additive_expression
             { $$ =mkbinaASTnode(emND_Lt, $1, $3 ); }
	| relational_expression '>'additive_expression
             { $$ =mkbinaASTnode(emND_Gt, $1, $3 ); }
	| relational_expression LE_OP additive_expression
             { $$ =mkbinaASTnode(emND_Le, $1, $3 ); }
	| relational_expression GE_OP additive_expression
             { $$ =mkbinaASTnode(emND_Ge, $1, $3 ); }
	;

equality_expression
	: relational_expression{ $$ = $1; }
	| equality_expression EQ_OP relational_expression
             { $$ =mkbinaASTnode(emND_Eq, $1, $3 ); }
	| equality_expression NE_OP relational_expression
             { $$ =mkbinaASTnode(emND_Ne, $1, $3 ); }
	;

logical_and_expression
	: equality_expression  { $$ = $1; }
	| logical_and_expression AND_OP equality_expression
             { $$ =mkbinaASTnode(emND_And, $1, $3 ); }
	;

logical_or_expression
	: logical_and_expression   { $$ = $1; }
	| logical_or_expression OR_OP logical_and_expression
             { $$ =mkbinaASTnode(emND_Or, $2, $3 ); }
	;

assignment_expression
	: logical_and_expression   { $$ = $1; }
	| unary_expression '=' assignment_expression
             { $$ = mkbinaASTnode(emND_Assign, $1, $3 ); }
	;


declaration: type_specifier init_declarator_list ';'   
	       { $$ = mkbinaASTnode(emND_Decltion, $1, $2); }
	   | error ';'  
	;

init_declarator_list
	: init_declarator   { $$ = $1; }
	| init_declarator_list ',' init_declarator  { $$ = glueASTnode( $1, $3); }
	;

init_declarator
	: declarator  { $$ = mkunaryASTnode( emND_InitDecltor, $1); }
	| declarator '=' assignment_expression { $$ = mkbinaASTnode( emND_InitDecltor, $1, $3); }
	;

type_specifier
        : STRING  { $$ = mkzeroASTnode(emND_String); }
	| VOID    { $$ = mkzeroASTnode(emND_VOID); }
	| CHAR    { $$ = mkzeroASTnode(emND_CHAR); }
	| SHORT   { $$ = mkzeroASTnode(emND_SHORT); }
	| INT     { $$ = mkzeroASTnode(emND_INT); }
	| struct_specifier  { $$ = $1; }
	;

struct_specifier
	: STRUCT IDENTIFIER '{' struct_declaration_list '}'  
            { $$ = mkbinaASTnode( emND_StructSpec, $2, $4); }
	| STRUCT '{' struct_declaration_list '}'  
            { $$ = mkunaryASTnode( emND_StructSpec, $3); }
	| STRUCT IDENTIFIER
            { $$ = mkunaryASTnode( emND_StructSpec, $2); }
	;


struct_declaration_list
	: struct_declaration     { $$ = $1; }
	| struct_declaration_list struct_declaration   { $$ = glueASTnode($1, $2); }
	;

struct_declaration
	: type_specifier declarator_list ';'  {$$=mkbinaASTnode(emND_StructDecl,$1,$2);}
	| error ';'
	;

declarator_list
	: declarator   { $$ = $1; }
	| declarator_list ',' declarator   { $$ = glueASTnode( $1, $3); }
	;



declarator
	: IDENTIFIER  { $$ = $1; } 
	| declarator '[' assignment_expression ']'
             {$$ = mkbinaASTnode( emND_ARRAY, $1, $3 ); }
	| declarator '(' parameter_list ')'
             {$$ = mkbinaASTnode(emND_Func, $1, $3 ); }
	| declarator '(' ')'
             {$$ = mkunaryASTnode(emND_Func, $1 ); }
	;

parameter_list
	: parameter_declaration   { $$ = mkunaryASTnode(emND_Para, $1); }
	| parameter_list ',' parameter_declaration  { 
                         $$ = mkbinaASTnode(emND_Para, $1, $3); }
	;

parameter_declaration
	: type_specifier declarator  
               { $$ = mkbinaASTnode( emND_Decltor, $1, $2 ); }
	;

identifier_list
	: IDENTIFIER    { $$ = $1; }
	| identifier_list ',' IDENTIFIER   { $$ = mkbinaASTnode( emND_InitLit, $1, $3 ); }
	;


statement
	: compound_statement     { $$ = mkunaryASTnode(emND_Stmt, $1); }
	| expression_statement   { $$ = mkunaryASTnode(emND_Stmt, $1); }
	| selection_statement    { $$ = mkunaryASTnode(emND_Stmt, $1); }
	| iteration_statement    { $$ = mkunaryASTnode(emND_Stmt, $1); }
	| jump_statement         { $$ = mkunaryASTnode(emND_Stmt, $1); }
	;

compound_statement
	: '{' '}'    { $$ = NULL; }
	| '{' block_item_list '}'    { $$ = mkunaryASTnode( emND_CompoStmt, $2 ); }
	;

block_item_list
	: block_item   { $$  = $1; }
	| block_item_list block_item   { $$ = glueASTnode( $1, $2 ); }
	;

block_item
	: declaration   { $$  = $1; }
	| statement   { $$  = $1; }
	;

expression_statement
	: ';'    { $$ = NULL; }
	| assignment_expression ';'  { $$ = mkunaryASTnode( emND_ExprStmt, $1 ); }
	| error ';'
	;

selection_statement
	: IF '(' assignment_expression ')' statement   
                  { $$ = mkquatASTnode( emND_If, $3, NULL, NULL, $5 ); }
	| IF '(' assignment_expression ')' statement ELSE statement   
                  { $$ = mkquatASTnode( emND_If,$3,NULL,$7,$5); }
	;

iteration_statement
	: WHILE '(' assignment_expression ')' statement  
                           { $$ = mkbinaASTnode( emND_While, $3, $5); }
	| FOR '(' expression_statement expression_statement ')' statement
                           { $$ = mkquatASTnode( emND_For, $3, $4, NULL, $6 ); }
	| FOR '(' expression_statement expression_statement assignment_expression ')' statement
                           { $$ = mkquatASTnode( emND_For, $3, $4, $5, $7 ); }
	| FOR '(' declaration expression_statement ')' statement
                           { $$ = mkquatASTnode( emND_For, $3, $4, NULL, $6 ); }
	| FOR '(' declaration expression_statement assignment_expression ')' statement  
                           { $$ = mkquatASTnode( emND_For, $3, $4, $5, $7 ); }
	;

jump_statement
	: CONTINUE ';'  { $$ = mkzeroASTnode(emND_Ctn); }
	| BREAK ';'     { $$ = mkzeroASTnode(emND_Brk); }
	| RETURN ';'    { $$ = mkzeroASTnode(emND_Ret); }
	| RETURN assignment_expression ';' { $$ = mkunaryASTnode(emND_Ret, $2); }
        | error ';'
	;


program_start : translation_unit { root = $1; }


translation_unit
	: external_declaration  { $$ = $1; }
	| translation_unit external_declaration  { $$ = glueASTnode( $1, $2 ); } 
	;

external_declaration
	: function_definition  { $$ = $1; }
	| declaration  { $$ = $1; }
	;

function_definition : type_specifier declarator compound_statement   
		    { $$ = mkTriASTnode( emND_Func, $1, $2, $3 ); }
	;


%%


ASTnode* glueASTnode( ASTnode* n1, ASTnode *n2 )
{
    return mkbinaASTnode( emND_GLUE, n1, n2 );
}


ASTnode* initASTNode()
{
    ASTnode *n = (ASTnode*)malloc(sizeof(ASTnode));
    if(!n)
        errorWithLineNo(emEC_Not_enough_memory);

    n->type = NULL;
    n->lhs = NULL;
    n->rhs = NULL;
    n->mid = NULL;
    n->stmt = NULL;
    n->lineno = yylineno;
    return n;
}



ASTnode* identASTnode( const char *name )
{
    ASTnode *n = initASTNode();
    n->op = emND_Ident;
    n->name = name;
    n->lineno = yylineno;
    return n;
}


ASTnode* constASTnode( int val )
{
    ASTnode *n = initASTNode();
    n->op = emND_Const;
    n->val = val;
    n->lineno = yylineno;
    return n;
}


ASTnode* strASTnode( std::string str )
{
    ASTnode *n = initASTNode();
    n->op = emND_String_Literal;
    n->str = str;
    n->lineno = yylineno;
    
    return n;
}


ASTnode* mkzeroASTnode( int op )
{
    return mkTriASTnode( op, NULL, NULL, NULL );
}



ASTnode* mkunaryASTnode( int op, ASTnode *lhs )
{
    return mkTriASTnode( op, lhs, NULL, NULL );
}


ASTnode* mkbinaASTnode( int op, ASTnode *lhs, ASTnode *rhs )
{
    return mkTriASTnode( op, lhs, NULL, rhs );
}


ASTnode* mkTriASTnode( int op, ASTnode *lhs, ASTnode *mid, ASTnode *rhs )
{
    ASTnode *node = initASTNode();
    node->op = op;
    node->lhs = lhs;
    node->mid = mid;
    node->rhs = rhs;
    return node;
}


ASTnode* mkquatASTnode( int op, ASTnode *lhs, ASTnode *mid, ASTnode *rhs, ASTnode *stmt )
{
    ASTnode *node = mkTriASTnode( op, lhs, mid, rhs );
    node->stmt = stmt;
    return node;
}


void yyerror( const char *format, ... )
{
    if( ErrorCnt < 30 ) {    //超过30个错，退出编译
        va_list vaList;
        va_start( vaList, format );
        
        fprintf(stderr, "Grammer Error at line %d column %d:",
             yylloc.first_line, yylloc.first_column );
        vfprintf( stderr, format, vaList );
        fprintf(stderr, "\n");
        va_end( vaList );
    }
    else {
        fclose(fout);
        exit(1);
    }
}

