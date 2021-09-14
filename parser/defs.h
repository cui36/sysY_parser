#ifndef _DEFS_H_
#define _DEFS_H_

typedef struct _BB BB;
typedef struct _IR IR;
typedef struct _Var Var;
typedef struct _Reg Reg;



/****** type struct  *********/
typedef struct _Type {
    int ty;
    int size;    //类型字节大小, 如果类型是数组则表示数组的容量大小(经过字节对齐)
    int align;   //字节对齐

    //Array
    struct _Type *ary_of;   //数组元素的类型
    int len;    //数组总的字节长度、字符串字符个数
    std::vector<int> dimens;  //数组定义时的维数
    
    //pointer
    struct _Type *ptr_to;
}Type;
/****** type struct end  *********/


/********  AST node struct  *******/
typedef struct _ASTnode {
    int op;
    int lineno;

    std::string name;
    int val;
    std::string str;

    Type *type;
    Var *var;

    struct _ASTnode *lhs;
    struct _ASTnode *mid;
    struct _ASTnode *rhs;

    struct _ASTnode *stmt;    // for(;;) stmt
} ASTnode;
/********  AST node struct end *******/



/******  Var struct  *******/
typedef struct _Var {
    std::string name;
    Type *type;
    bool is_local;
    int para;   //-1不是参数，否则表示第几个参数

    int off;    //for rbp
    
    bool take_addr;   //优化

    int globVal;
    std::string globStr;

    Reg* promoted;   //for optimize
}Var;
/******  Var struct end *******/



/*********  function struct *********/
typedef struct _Function {
    std::string name;
    Type *retType;

    std::vector<ASTnode*> para;  //形参
    std::vector<Var*> localVars;

    ASTnode *body;
    std::vector<BB*> bbs;
}Function;
/*********  function struct end *********/



/*******  prog struct  **********/
typedef struct _Program{
    std::vector<Var*> gVars;
    std::vector<Function*> gFuncs;
} Program;
/*******  prog struct end **********/



/*********** stack struct  ************/
typedef struct _Stack {
    std::map<std::string, Var*> vars;
}Stack;
/*********** stack struct end  ************/



/********** reg struct *************/
typedef struct _Reg{
    int vn;   //virtual register number
    int rn;   //real register number

    //分配寄存器
    int def;
    int last_use;
    bool spill;
    int para;   //-1不是参数，否则表示第几个参数

    Var *var;    

    Reg* promoted;   //for opt
}Reg;
/********** reg struct end *************/



/********** basic block struct *************/
typedef struct _BB{
    int label;
    std::vector<IR*> vec_ir;
}BB;
/********** basic block struct end *************/




/********** IR struct *************/
typedef struct _IR{
    int op;
    Reg *r0;
    Reg *r1;
    Reg *r2;

    int imm;
    Var *var;
    std::string name;  
    int size;  //类型大小
    int relative;    //循环体使用

    Reg *idx;  //寄存器的值表示数组元素偏移

    BB *bb1;   //成功跳转的基本块
    BB *bb2;   //失败跳转的基本块

    int debug; //for debug

    std::vector<struct _Reg*> args;   //函数参数
} IR;
/********** IR struct end *************/


/**********  IR enum  ***********/
enum {
    IR_ADD = 1,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_IMM,   //5
    IR_BPREL,
    IR_MOV,
    IR_RETURN,
    IR_CALL,
    IR_LABEL_ADDR,  //10
    IR_EQ,
    IR_NE,
    IR_LE,
    IR_LT,
    IR_GT,
    IR_GE,
    IR_AND,
    IR_OR,
    IR_XOR,
    IR_SHL,
    IR_SHR,
    IR_MOD,
    IR_JMP,
    IR_BR,
    IR_LOAD,   //25
    IR_LOAD_SPILL,
    IR_STORE,   //27
    IR_STORE_ARG,
    IR_STORE_SPILL,
    IR_NOP,
    IR_INC,
    IR_DEC,
    IR_Unary_Minus,
    IR_PARA,    //34
    IR_SCALE,    //数据大小缩放
};
/**********  IR enum end  ***********/



/************   error code enum  **************/
typedef enum _ERROR_CODE
{
    emEC_Not_enough_memory = 0,
    emEC_Syntax_error,
    emEC_Unterminated_comment,
    emEC_Unknown_character,
    emEC_Miss_Terminating_character,
    emEC_Invalid_specifier,
    emEC_Function_redefined,
    emEC_Undeclared_identifier,
    emEC_Left_operand_must_be_l_value,
    emEC_Illegal_break,
    emEC_Illegal_continue,
    emEC_Redefinition,
    emEC_Void_cannot_be_an_argument_type,
    emEC_No_main_function,
    emEC_Void_function_returning_a_value,
    emEC_Must_return_a_value,
    emEC_Expected_constant_expression,
    emEC_Subscript_value_is_not_array,
    emEC_Cannot_open_file,
    emEC_Too_few_argumnet,
    emEC_Too_many_argument,
    emEC_Omit_Parameter_Name,
    emEC_Invalid_Initializer,
} ERROR_CODE;

static const char* ErrorInfo[] = {
    "internal error, not enough memory",
    "Syntax error on line %d",
    "unterminated comment",
    "meet a unknown character %c",
    "miss terminating %c character",
    "invalid specifier",
    "function '%s' redefined",
    "'%s' : undeclared identifier",
    "left operand must be l-value",
    "illegal break",
    "illegal continue",
    "'%s' : redefinition",
    "'void' cannot be an argument type, except for '(void)'",
    "no 'main' function",
    "'%s' : 'void' function returning a value",
    "'%s' : must return a value",
    "expected constant expression",
    "subscripted value is not array",
    "Cannot open file: '%s': No such file or directory",
    "Too few argumnets for %s",
    "Too many arguments for %s",
    "Parameter name omitted",
    "Invalid initializer",
};
/************  error code enum end   ************/



/********** type specifier enum  ***********/
typedef enum _TYPE_SPEC
{
    emTYPE_UNKNOWN = 0,
    
    emTYPE_SPEC_Mask = 0x10,
    emTYPE_VOID,
    emTYPE_CHAR,
    emTYPE_SHORT,
    emTYPE_INT,
    emTYPE_STRING,
    
    emTYPE_POINTER_Mask = 0x20,
    emTYPE_POINTER,
    emTYPE_ARRAY,   //34

} TYPE_SPEC;
/********** type specifier enum end  ***********/


/**************  ASTnode enum   **************/
typedef enum _NODE_TYPE {
    emND_UNKNOWN = 0,
    emND_GLUE,
    
    emND_Const,
    emND_Ident,   //3
    emND_Decl,
    emND_List,
    emND_Cond,
    emND_String_Literal,

    emND_Start,

    //jmp
    emND_Brk,
    emND_Ctn,

    //stmt
    emND_Stmt,  // 11
    emND_For,
    emND_While,
    emND_If,
    emND_CompoStmt, //15
    emND_ExprStmt,
    emND_SelStmt,
    emND_IterStmt,
    emND_JmpStmt,

    //expr
    emND_ArgExpr,   //20
    emND_UnaryExpr,  //21
    emND_Inc,
    emND_Dec,
    emND_PostExpr,
    emND_PtrOp,  //->
    emND_Unary_Addr,
    emND_Unary_Star,
    emND_Unary_Plus,
    emND_Unary_Minus,

    //struct
    emND_StructDeclLit,  //30
    emND_StructDecl,
    emND_StructSpec,

    //init
    emND_DesgtorLit,
    emND_Desgtion,
    emND_Desgtor,
    emND_Comma,
    emND_LParen,   //37
    emND_RParen,
    emND_LSquBrck,
    emND_RSquBrck,
    emND_LBracket,
    emND_RBracket,
    emND_Equal,  //符号
    emND_InitLit,
    emND_Initzer,

    //declarator
    emND_DirtAbsDecl,
    emND_AbsDecl,
    emND_Decltor,  //48
    emND_InitDecltor,
    emND_Decltion,

    //function
    emND_Func,
    emND_Call,   //52
    emND_Ret,
    emND_Para,
    
    //type specifier
    emND_VOID,
    emND_CHAR,
    emND_SHORT,
    emND_INT,
    emND_POINTER,
    emND_ARRAY,

    //emND_Binary
    emND_Assign,   // 61
    emND_MUL_ASSIGN,
    emND_DIV_ASSIGN,
    emND_MOD_ASSIGN,
    emND_ADD_ASSIGN,
    emND_SUB_ASSIGN,
    emND_LEFT_ASSIGN,
    emND_RIGHT_ASSIGN,
    emND_AND_ASSIGN,
    emND_XOR_ASSIGN,  //70
    emND_OR_ASSIGN  ,
    emND_Mul,
    emND_Div,
    emND_Mod,
    emND_Plus,
    emND_Minus,
    emND_Shl,
    emND_Shr,
    emND_And,
    emND_Xor,  //80
    emND_Or,
    
    //emND_Unary
    emND_Neg,
    emND_Not,
    emND_LNot,
    emND_LAnd,
    emND_LOr,
    
    //Compare
    emND_Eq,
    emND_Ne,
    emND_Lt,
    emND_Gt,
    emND_Le,
    emND_Ge,

    emND_String,
    emND_ADDR,
} NODE_TYPE;
/*************  ASTnode enum end  ***************/



#endif
