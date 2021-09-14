.text
.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
	leaq -4(%rbp), %r10
	movq $0, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	movq $0, %r10
	movq %r10, %r11
	movslq %r11d, %r11
.L2:
	leaq -4(%rbp), %r10
	movslq (%r10), %rbx
	movq $100, %r10
	cmpq %r10, %rbx
	setl %bl
	movzbq %bl, %rbx
	cmpq $0, %rbx
	jne .L3
	jmp .L4
.L3:
.L5:
	leaq -4(%rbp), %r10
	movslq (%r10), %rbx
	movq $50, %r10
	cmpq %r10, %rbx
	sete %bl
	movzbq %bl, %rbx
	cmpq $0, %rbx
	jne .L6
	jmp .L7
.L6:
	leaq -4(%rbp), %r10
	leaq -4(%rbp), %rbx
	movslq (%rbx), %r12
	movq $1, %rbx
	addq %rbx, %r12
	movl %r12d, (%r10)
	movslq %r10d, %r10
	jmp .L2
	jmp .L7
.L7:
	movq %r11, %r10
	leaq -4(%rbp), %rbx
	movslq (%rbx), %r12
	addq %r12, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	leaq -4(%rbp), %r10
	leaq -4(%rbp), %rbx
	movslq (%rbx), %r12
	movq $1, %rbx
	addq %rbx, %r12
	movl %r12d, (%r10)
	movslq %r10d, %r10
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
.Lend8:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
