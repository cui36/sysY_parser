#ifndef _DATA_H_
#define _DATA_H_

#ifndef extern_
#define extern_ extern
#endif

extern int yylineno;
extern FILE *yyin;
extern int num_regs;

// extern作用：将变量声明至全局空间
extern_ FILE *fout;  
extern_ Program prog;  //保存全局变量和函数
extern_ ASTnode *root;
extern_ int nlabel;
extern_ int nreg;
extern_ int ErrorCnt;

extern_ std::map<std::string, Function* > buil_in_funcs;
#endif
