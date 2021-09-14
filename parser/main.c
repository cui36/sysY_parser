#include "decl.h"  // 声明函数
#include "defs.h"  // 各种定义
#define extern_
#include "data.h"   // 各种全局变量
#undef extern_  //  没有定义，参考


static void init();



int main( int argc, char *argv[] ) {
    init();

    if(argc < 2)
        error(emEC_Too_few_argumnet, "main");

    if( !(yyin = fopen( argv[1], "r" )) )
        error(emEC_Cannot_open_file, argv[1]);

    //if( !(fout = fopen("log.S", "w")) )
        //error(emEC_Cannot_open_file, "log");
    fout = stdout;

    yyparse();  // 语法分析器（里面调用词法分析器，在parser.tab.c里面定义yylex()(在lex.yy.c中定义)），parser.tab.h里定义，自动调用flex，
    if( ErrorCnt == 0 ){
      parseASTTree( root );  // 语义分析，root是抽象语法树的根结点
      if( ErrorCnt == 0 ){l
          gen_ir();
	  if( ErrorCnt == 0 ){
              opt();
              alloc_regs();
              gen_x86();
	  }
      }
    }


    fclose( yyin );
    fclose( fout );

    return 0;
}


void init()
{
    fout = NULL;
    root = NULL;
    nlabel = 1;
    nreg = 1;
    ErrorCnt = 0;

    //第三方函数和自定义函数的参数管理方式不一样
    //需要另外保存好寄存器环境
    Function *func = new Function();  
    func->name = "printint";
    func->retType = void_type();
    buil_in_funcs["printint"] = func;
    func->para.push_back( identASTnode("para1") );
    func->para[0]->type = int_type();

    Function *func2 = new Function();
    func2->name = "printstr";
    func2->retType = void_type();
    buil_in_funcs["printstr"] = func2;
    func2->para.push_back( identASTnode("para1") );
    func2->para[0]->type = pointer_type( char_type() );

    Function *func3 = new Function();
    func3->name = "readint";
    func3->retType = void_type();
    buil_in_funcs["readint"] = func3;
    func3->para.push_back( identASTnode("para1") );
    func3->para[0]->type = pointer_type( int_type() );

}

