	.globl a
	.data
	.align 4
	.size a, 4
a:
	.int 0
.text
.global func
func:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
	movq $1, %r10
	subq %r10, %rdi
	movl %edi, %edi
	movslq %edi, %rdi
	movslq %edi, %rdi
.L2:
	movq %rdi, %rax
	jmp .Lend4
.Lend4:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
.text
.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L3:
	leaq a(%rip), %r10
	movq $10, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq a(%rip), %r10
	movslq (%r10), %r11
	pushq %rbx
	pushq %r10
	pushq %r11
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	pushq %rdi
	pushq %rsi
	pushq %rdx
	pushq %rcx
	pushq %r8
	pushq %r9
	movq %r11, %rdi
	movslq %edi, %rdi
	call func
	popq %r9
	popq %r8
	popq %rcx
	popq %rdx
	popq %rsi
	popq %rdi
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %r11
	popq %r10
	popq %rbx
	movq %rax, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	movq %r11, %r10
	pushq %rbx
	pushq %r10
	pushq %r11
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	pushq %rdi
	pushq %rsi
	pushq %rdx
	pushq %rcx
	pushq %r8
	pushq %r9
	movq %r10, %rdi
	movslq %edi, %rdi
	call printint
	popq %r9
	popq %r8
	popq %rcx
	popq %rdx
	popq %rsi
	popq %rdi
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %r11
	popq %r10
	popq %rbx
	movq %rax, %r11
.Lend5:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
