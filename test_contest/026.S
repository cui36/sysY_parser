	.globl a
	.data
	.align 4
	.size a, 4
a:
	.int 0
.text
.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	pushq %rbx


.L1:
	leaq a(%rip), %r10
	movq $10, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
.L2:
	leaq a(%rip), %r10
	movslq (%r10), %r11
	movq $0, %r10
	cmpq %r10, %r11
	setg %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L3
	jmp .L4
.L3:
	movq $1, %r10
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
	jmp .L4
.L4:
.L5:
	movq $0, %r10
	movq %r10, %rax
	jmp .Lend6
.Lend6:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
