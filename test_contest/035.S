.text
.global FourWhile
FourWhile:
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
	movq $6, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq -12(%rbp), %r10
	movq $7, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq -16(%rbp), %r10
	movq $10, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
.L2:
	leaq -4(%rbp), %r10
	movslq (%r10), %r11
	movq $20, %r10
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
	movq $3, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L5:
	leaq -8(%rbp), %r10
	movslq (%r10), %r11
	movq $10, %r10
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
	movq $1, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L8:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $7, %r10
	cmpq %r10, %r11
	sete %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L9
	jmp .L10
.L9:
	leaq -12(%rbp), %rbx
	leaq -12(%rbp), %r12
	movslq (%r12), %r13
	movq $1, %r12
	subq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L11:
	leaq -16(%rbp), %r10
	movslq (%r10), %r11
	movq $20, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L12
	jmp .L13
.L12:
	leaq -16(%rbp), %rbx
	leaq -16(%rbp), %r12
	movslq (%r12), %r13
	movq $3, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
	jmp .L11
.L13:
	leaq -16(%rbp), %r10
	leaq -16(%rbp), %r11
	movslq (%r11), %rbx
	movq $1, %r11
	subq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L8
.L10:
	leaq -12(%rbp), %r10
	leaq -12(%rbp), %r11
	movslq (%r11), %rbx
	movq $1, %r11
	addq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L5
.L7:
	leaq -8(%rbp), %r10
	leaq -8(%rbp), %r11
	movslq (%r11), %rbx
	movq $2, %r11
	subq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L2
.L4:
.L14:
	leaq -4(%rbp), %r10
	movslq (%r10), %r11
	leaq -8(%rbp), %r10
	movslq (%r10), %rbx
	leaq -16(%rbp), %r10
	movslq (%r10), %r12
	addq %r12, %rbx
	addq %rbx, %r11
	leaq -12(%rbp), %r10
	movslq (%r10), %rbx
	addq %rbx, %r11
	movq %r11, %rax
	jmp .Lend16
.Lend16:
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


.L15:
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
	call FourWhile
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
.Lend17:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
