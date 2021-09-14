	.globl N
	.data
	.align 4
	.size N, 4
N:
	.int 1
	.comm arr,32,16
.text
.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	pushq %rbx


.L1:
	movq $1, %r10
	movq $0, %r11
	movq $1, %rbx
	imulq %rbx, %r11
	addq %r11, %r10
	movq $4, %r11
	imulq %r11, %r10
	movq $1, %r11
	movq $0, %rbx
	movq $1, %r12
	imulq %r12, %rbx
	addq %rbx, %r11
	movq $4, %rbx
	imulq %rbx, %r11
	leaq arr(%rip), %rbx
	addq %r11, %rbx
	movq $1, %r11
	movl %r11d, (%rbx)
	movslq %ebx, %rbx
	movq $1, %r10
	movq $1, %r11
	movq $1, %rbx
	imulq %rbx, %r11
	addq %r11, %r10
	movq $4, %r11
	imulq %r11, %r10
	movq $1, %r11
	movq $1, %rbx
	movq $1, %r12
	imulq %r12, %rbx
	addq %rbx, %r11
	movq $4, %rbx
	imulq %rbx, %r11
	leaq arr(%rip), %rbx
	addq %r11, %rbx
	movq $2, %r11
	movl %r11d, (%rbx)
	movslq %ebx, %rbx
	movq $1, %r10
	movq $2, %r11
	movq $1, %rbx
	imulq %rbx, %r11
	addq %r11, %r10
	movq $4, %r11
	imulq %r11, %r10
	movq $1, %r11
	movq $2, %rbx
	movq $1, %r12
	imulq %r12, %rbx
	addq %rbx, %r11
	movq $4, %rbx
	imulq %rbx, %r11
	leaq arr(%rip), %rbx
	addq %r11, %rbx
	movq $33, %r11
	movl %r11d, (%rbx)
	movslq %ebx, %rbx
	movq $1, %r10
	movq $3, %r11
	movq $1, %rbx
	imulq %rbx, %r11
	addq %r11, %r10
	movq $4, %r11
	imulq %r11, %r10
	movq $1, %r11
	movq $3, %rbx
	movq $1, %r12
	imulq %r12, %rbx
	addq %rbx, %r11
	movq $4, %rbx
	imulq %rbx, %r11
	leaq arr(%rip), %rbx
	addq %r11, %rbx
	movq $4, %r11
	movl %r11d, (%rbx)
	movslq %ebx, %rbx
	movq $1, %r10
	movq $4, %r11
	movq $1, %rbx
	imulq %rbx, %r11
	addq %r11, %r10
	movq $4, %r11
	imulq %r11, %r10
	movq $1, %r11
	movq $4, %rbx
	movq $1, %r12
	imulq %r12, %rbx
	addq %rbx, %r11
	movq $4, %rbx
	imulq %rbx, %r11
	leaq arr(%rip), %rbx
	addq %r11, %rbx
	movq $5, %r11
	movl %r11d, (%rbx)
	movslq %ebx, %rbx
	movq $1, %r10
	movq $5, %r11
	movq $1, %rbx
	imulq %rbx, %r11
	addq %r11, %r10
	movq $4, %r11
	imulq %r11, %r10
	movq $1, %r11
	movq $5, %rbx
	movq $1, %r12
	imulq %r12, %rbx
	addq %rbx, %r11
	movq $4, %rbx
	imulq %rbx, %r11
	leaq arr(%rip), %rbx
	addq %r11, %rbx
	movq $6, %r11
	movl %r11d, (%rbx)
	movslq %ebx, %rbx
	leaq -4(%rbp), %r10
	movq $0, %r11
	movl %r11d, (%r10)
	movq $0, %r10
	movq %r10, %r11
.L2:
	leaq -4(%rbp), %r10
	movslq (%r10), %rbx
	movq $6, %r10
	cmpq %r10, %rbx
	setl %bl
	movzbq %bl, %rbx
	cmpq $0, %rbx
	jne .L3
	jmp .L4
.L3:
	movq %r11, %r12
	movq $1, %r13
	leaq -4(%rbp), %r15
	movslq (%r15), %r14
	movq $1, %r15
	imulq %r15, %r14
	addq %r14, %r13
	movq $4, %r14
	imulq %r14, %r13
	leaq arr(%rip), %r14
	addq %r13, %r14
	movslq (%r14), %r13
	addq %r13, %r12
	movq %r12, %r11
	movslq %r11d, %r11
	leaq -4(%rbp), %r12
	leaq -4(%rbp), %r13
	movslq (%r13), %r14
	movq $1, %r13
	addq %r13, %r14
	movl %r14d, (%r12)
	movslq %r12d, %r12
	jmp .L2
.L4:
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
