.text
.global doubleWhile
doubleWhile:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
	leaq -4(%rbp), %r10
	movq $5, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq -8(%rbp), %r10
	movq $7, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
.L2:
	leaq -4(%rbp), %r10
	movslq (%r10), %r11
	movq $100, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L3
	jmp .L4
.L3:
	leaq -4(%rbp), %rbx
	leaq -4(%rbp), %r12
	movslq (%r12), %r13
	movq $30, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L5:
	leaq -8(%rbp), %r10
	movslq (%r10), %r11
	movq $100, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L6
	jmp .L7
.L6:
	leaq -8(%rbp), %rbx
	leaq -8(%rbp), %r12
	movslq (%r12), %r13
	movq $6, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
	jmp .L5
.L7:
	leaq -8(%rbp), %r10
	leaq -8(%rbp), %r11
	movslq (%r11), %rbx
	movq $100, %r11
	subq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L2
.L4:
.L8:
	leaq -8(%rbp), %r10
	movslq (%r10), %r11
	movq %r11, %rax
	jmp .Lend10
.Lend10:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
.text
.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	pushq %rbx


.L9:
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
	call doubleWhile
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
.Lend11:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
