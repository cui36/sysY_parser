	.globl g
	.data
	.align 4
	.size g, 4
g:
	.int 0
	.globl h
	.data
	.align 4
	.size h, 4
h:
	.int 0
	.globl f
	.data
	.align 4
	.size f, 4
f:
	.int 0
	.globl e
	.data
	.align 4
	.size e, 4
e:
	.int 0
.text
.global EightWhile
EightWhile:
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
.L14:
	leaq e(%rip), %r10
	movslq (%r10), %r11
	movq $1, %r10
	cmpq %r10, %r11
	setg %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L15
	jmp .L16
.L15:
	leaq e(%rip), %rbx
	leaq e(%rip), %r12
	movslq (%r12), %r13
	movq $1, %r12
	subq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L17:
	leaq f(%rip), %r10
	movslq (%r10), %r11
	movq $2, %r10
	cmpq %r10, %r11
	setg %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L18
	jmp .L19
.L18:
	leaq f(%rip), %rbx
	leaq f(%rip), %r12
	movslq (%r12), %r13
	movq $2, %r12
	subq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L20:
	leaq g(%rip), %r10
	movslq (%r10), %r11
	movq $3, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L21
	jmp .L22
.L21:
	leaq g(%rip), %rbx
	leaq g(%rip), %r12
	movslq (%r12), %r13
	movq $10, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
.L23:
	leaq h(%rip), %r10
	movslq (%r10), %r11
	movq $10, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L24
	jmp .L25
.L24:
	leaq h(%rip), %rbx
	leaq h(%rip), %r12
	movslq (%r12), %r13
	movq $8, %r12
	addq %r12, %r13
	movl %r13d, (%rbx)
	movslq %ebx, %rbx
	jmp .L23
.L25:
	leaq h(%rip), %r10
	leaq h(%rip), %r11
	movslq (%r11), %rbx
	movq $1, %r11
	subq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L20
.L22:
	leaq g(%rip), %r10
	leaq g(%rip), %r11
	movslq (%r11), %rbx
	movq $8, %r11
	subq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L17
.L19:
	leaq f(%rip), %r10
	leaq f(%rip), %r11
	movslq (%r11), %rbx
	movq $1, %r11
	addq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L14
.L16:
	leaq e(%rip), %r10
	leaq e(%rip), %r11
	movslq (%r11), %rbx
	movq $1, %r11
	addq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
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
.L26:
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
	leaq e(%rip), %r10
	movslq (%r10), %rbx
	leaq -16(%rbp), %r10
	movslq (%r10), %r12
	addq %r12, %rbx
	leaq g(%rip), %r10
	movslq (%r10), %r12
	subq %r12, %rbx
	leaq h(%rip), %r10
	movslq (%r10), %r12
	addq %r12, %rbx
	subq %rbx, %r11
	movq %r11, %rax
	jmp .Lend28
.Lend28:
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


.L27:
	leaq g(%rip), %r10
	movq $1, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq h(%rip), %r10
	movq $2, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq e(%rip), %r10
	movq $4, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
	leaq f(%rip), %r10
	movq $6, %r11
	movl %r11d, (%r10)
	movslq %r10d, %r10
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
	call EightWhile
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
.Lend29:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
