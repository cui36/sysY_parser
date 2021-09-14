#include "decl.h"
#include "defs.h"
#include "data.h"


const char* x86_regs[] = 
   {"%r10", "%r11", "%rbx", "%r12", "%r13", "%r14", "%r15",
	   "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
const char* x86_regs8[] = 
   {"%r10b", "%r11b", "%bl", "%r12b", "%r13b", "%r14b", "%r15b", 
	   "%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
const char* x86_regs16[] = 
   {"%r10w", "%r11w", "%bx", "%r12w", "%r13w", "%r14w", "%r15w",
	   "%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
const char* x86_regs32[] = 
   {"%r10d", "%r11d", "%ebx", "%r12d", "%r13d", "%r14d", "%r15d",
	   "%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};


const char *argregs[] = 
    {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
const char *argregs8[] = 
    {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
const char *argregs16[] = 
    {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
const char *argregs32[] = 
    {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};

int num_regs = ( sizeof(x86_regs) - sizeof(argregs) ) / sizeof(char*);


const char *suffix[] = {"b", "w", "l", "q"};

const char *cmplist[] = { "sete", "setne", "setle", "setl", "setg", "setge" };


static void save_regs();
static void recover_regs();



const char* get_type( Type *ty )
{
    static const char *type_spe[] = { "byte", "short", "int", "string"};
    switch( ty->ty ){
      case emTYPE_CHAR:
	  return type_spe[ty->ty - emTYPE_CHAR];
      case emTYPE_SHORT:
	  return type_spe[ty->ty - emTYPE_CHAR];
      case emTYPE_INT:
	  return type_spe[ty->ty - emTYPE_CHAR];
      case emTYPE_STRING:
	  return type_spe[ty->ty - emTYPE_CHAR];
      case emTYPE_ARRAY:
	  return get_type( ty->ary_of );
      default:
	  assert( 0 && "Not exists this type" );
    }

}


const char* ins_suffix( int size )
{
    switch( size ){
        case 1: return suffix[0];
        case 2: return suffix[1];
        case 4: return suffix[2];
        case 8: return suffix[3];
	default: assert( 0 && "size of type wrong" );
    }
}


static void p( const char *format, ... )
{
    va_list args;

    va_start(args, format);
    vfprintf( fout, format, args );
    va_end(args);

    fprintf(fout, "\n");
}



static void emit( const char *format, ... )
{
    va_list args;

    fprintf(fout, "\t");

    va_start(args, format);
    vfprintf( fout, format, args );
    va_end(args);

    fprintf(fout, "\n");
}


static void emit_data( Var *globVar )
{
   if( globVar->type->ty == emTYPE_ARRAY ){
       emit(".comm %s,%d,%d", globVar->name.data(),
	  globVar->type->size, globVar->type->align );
   }
   else{
       emit(".globl %s", globVar->name.data() );
       emit(".data");
       emit(".align %d", globVar->type->align);
       emit(".size %s, %d", globVar->name.data(), globVar->type->size );
    
       p("%s:", globVar->name.data());
    
       if( globVar->type->ty == emTYPE_STRING )
         emit(".string \"%s\"", globVar->globStr.data() );
       else
         emit(".%s %d", get_type( globVar->type ), globVar->globVal );
   }
}


int roundup( int off, int align )
{
    return  ( off + (align - 1) ) & ~(align - 1);
}


const char* choose_reg( int rn, int size )
{
    switch( size ){
        case 1: return x86_regs8[rn];
        case 2: return x86_regs16[rn];
        case 4: return x86_regs32[rn];
        case 8: return x86_regs[rn];
	default: assert( 0 && "size of register wrong" );
    }
}


void emit_ir( IR *ir, const char *Lend )
{
    int r0 = ir->r0 ? ir->r0->rn : 0;
    int r1 = ir->r1 ? ir->r1->rn : 0;
    int r2 = ir->r2 ? ir->r2->rn : 0;

    //printf("id : %d, ", ir->debug);
    switch( ir->op ){
        case IR_IMM:
            emit("movq $%d, %s", ir->imm, x86_regs[r0]);
	    break;
	case IR_BPREL:
	    if( ir->idx == NULL )
	      emit("leaq %d(\%rbp), %s", ir->var->off, x86_regs[r0] );
	    else   //数组
	      emit("leaq %d(\%rbp, %s,), %s", ir->var->off, x86_regs[ir->idx->rn], 
	          x86_regs[r0] );
	    break;
	case IR_LABEL_ADDR:
	    if( ir->idx == NULL )
	      emit("leaq %s(\%rip), %s", ir->name.data(), x86_regs[r0]);
	    else {
	      emit("leaq %s(\%rip), %s", ir->name.data(), x86_regs[r0]);
	      emit("addq %s, %s", x86_regs[ir->idx->rn], x86_regs[r0]);
            }
	    break;
	case IR_MOV:
	    emit("movq %s, %s", x86_regs[r2], x86_regs[r0]);
	    break;
	case IR_STORE_SPILL:
	    //emit("movq %s, %d(%%rbp)", x86_regs[r1], ir->var->off);
            emit("mov%s %s, %d(%%rbp)",ins_suffix(ir->size), choose_reg(r1, ir->size),
	         ir->var->off );
	    break;
	case IR_LOAD_SPILL:
	    //emit("movq %d(%%rbp), %s", ir->var->off, x86_regs[r0]);
	    emit("movs%sq %d(%%rbp), %s", ins_suffix(ir->size), ir->var->off,
	        x86_regs[r0] );
	    break;
	case IR_STORE:
	    if( ir->r1->para == -1 ) {
	       emit("mov%s %s, (%s)",ins_suffix(ir->size), choose_reg(r2, ir->size),
		 x86_regs[r1] );
	    } else {
	       //emit("movq %s, %s", x86_regs[r2], x86_regs[r1] );
	       emit("mov%s %s, %s",ins_suffix(ir->size), choose_reg(r2, ir->size),
		          choose_reg(r1, ir->size) );
	       emit("movs%sq %s, %s", ins_suffix(ir->size), choose_reg(r1,ir->size), 
			 x86_regs[r1]);
	    }
	    break;
	case IR_SCALE:
	     emit("movs%sq %s, %s", ins_suffix(ir->size), choose_reg(r1,ir->size), 
		 x86_regs[r1]);
             break;
	case IR_RETURN:
	    emit("movq %s, %%rax", x86_regs[r2]);
	    emit("jmp %s", Lend);
	    break;
	case IR_ADD:
	    emit("addq %s, %s", x86_regs[r2], x86_regs[r1]);
	    break;
	case IR_LOAD:
	    if( ir->r2->para == -1 ) {
	      emit("movs%sq (%s), %s", ins_suffix(ir->size), x86_regs[r2],
		    x86_regs[r0] );
	    } else
	      emit("movq %s, %s", x86_regs[r2], x86_regs[r0] );
	    break;
	case IR_CALL: {
	    save_regs();
            Function *func = lookforFunc( ir->name.data() );
	    for( int i = 0;  i < ir->args.size();  i++){
		int size = func->para[i]->type->size;
	        emit("movq %s, %s", x86_regs[ir->args[i]->rn], argregs[i]);
		if( size < 8 )
	            emit("movs%sq %s, %s", ins_suffix(size), choose_reg(i+num_regs,size), 
			 argregs[i]);
	    }
	    emit("call %s", ir->name.data() );
	    recover_regs();
	    emit("movq %rax, %s", x86_regs[r0]);
	    break;
        }
        case IR_NOP:
	case IR_PARA:
            break;
	case IR_SUB:
            emit("subq %s, %s", x86_regs[r2], x86_regs[r1]);
	    break;
	case IR_MUL:
            emit("imulq %s, %s", x86_regs[r2], x86_regs[r1]);
	    break;
	case IR_DIV:
            emit("movq %s, %rax", x86_regs[r1]);
	    emit("cqo");
	    emit("idivq %s", x86_regs[r2]);
	    emit("movq %rax, %s", x86_regs[r1]);
	    break;
	case IR_MOD:
            emit("movq %s, %rax", x86_regs[r1]);
	    emit("cqo");
	    emit("idivq %s", x86_regs[r2]);
	    emit("movq %rdx, %s", x86_regs[r1]);
	    break;
	case IR_EQ:
	case IR_NE:
	case IR_GE:
	case IR_GT:
	case IR_LE:
	case IR_LT:
	    emit("cmpq %s, %s",  x86_regs[r2], x86_regs[r1] );
	    emit("%s %s", cmplist[ir->op - IR_EQ], x86_regs8[r1]);
	    emit("movzbq %s, %s", x86_regs8[r1], x86_regs[r1]);
	    break;
	case IR_Unary_Minus:
	    emit("neg %s", x86_regs[r0]);
	    break;
	case IR_JMP:
	    emit("jmp .L%d", ir->bb1->label);
	    break;
	case IR_BR:
            emit("cmpq $0, %s", x86_regs[r2]);
	    emit("jne .L%d", ir->bb1->label);
	    emit("jmp .L%d", ir->bb2->label);
	    break;
	case IR_INC:
	    emit("addq $%d, %s", ir->imm, x86_regs[r1]);
	    break;
	case IR_DEC:
	    emit("subq $%d, %s", ir->imm, x86_regs[r1]);
	    break;
	default:
	    assert( 0 && "Unknown IR operator");
    }
}


//环境保存
void save_regs()
{
    emit("pushq %rbx");
    emit("pushq %r10");
    emit("pushq %r11");
    emit("pushq %r12");
    emit("pushq %r13");
    emit("pushq %r14");
    emit("pushq %r15");

    emit("pushq %rdi");
    emit("pushq %rsi");
    emit("pushq %rdx");
    emit("pushq %rcx");
    emit("pushq %r8");
    emit("pushq %r9");
}


void recover_regs()
{
    emit("popq %r9");
    emit("popq %r8");
    emit("popq %rcx");
    emit("popq %rdx");
    emit("popq %rsi");
    emit("popq %rdi");

    emit("popq %r15");
    emit("popq %r14");
    emit("popq %r13");
    emit("popq %r12");
    emit("popq %r11");
    emit("popq %r10");
    emit("popq %rbx");
}


void emit_code( Function *fn )
{
    int off = 0;   //计算局部变量存放在栈帧中的偏移地址(相对rbp)
    std::string endLabel = ".Lend" + std::to_string(nlabel++);

    for( int i = 0;  i < fn->localVars.size();  i++ ){
	Var *var = fn->localVars[i];
        off += var->type->size;                  //数组对齐 32 16 8 4
	off = roundup( off, var->type->align );  //字节对齐
	var->off = -off;
    }


    p(".text");
    p(".global %s", fn->name.data() );
    p("%s:", fn->name.data() );
    emit("pushq %rbp");
    emit("movq %rsp, %rbp");
    emit("subq $%d, %rsp", roundup(off, 16) );  //可优化

    //save_regs();
    emit("pushq %rbx");
    p("\n");

    for( int i = 0;  i < fn->bbs.size();  i++ ){
        BB *bb = fn->bbs[i];
	p(".L%d:", bb->label);
	for( int i = 0;  i < bb->vec_ir.size();  i++ ){
	    IR *ir = bb->vec_ir[i];
	    emit_ir(ir, endLabel.data() );
	}
    }

    
    p( "%s:", endLabel.data() );
    //recover_regs();
    emit("popq %rbx");

    emit("movq %rbp, %rsp");
    emit("popq %rbp");
    emit("ret");
}



void gen_x86( )
{
    for( int i = 0;  i < prog.gVars.size(); i++ ){
        emit_data( prog.gVars[i] );
    }
    for( int i = 0;  i < prog.gFuncs.size();  i++ ){
        emit_code( prog.gFuncs[i] );
    }
}
