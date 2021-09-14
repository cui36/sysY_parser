.text
.global deepWhileBr
deepWhileBr:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	pushq %rbx


.L1:
	leaq -12(%rbp), %r10
	addq %rsi, %rdi
	movl %edi, (%r10)
	movslq %r10d, %r10
.L2:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $75, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L3
	jmp .L4
.L3:
	movq $42, %rbx
	movq %rbx, %r12
	movslq %r12d, %r12
.L5:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $100, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L6
	jmp .L7
.L6:
	leaq -12(%rbp), %r10
	leaq -12(%rbp), %r11
	movslq (%r11), %rbx
	movq %r12, %r11
	addq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
.L8:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $99, %r10
	cmpq %r10, %r11
	setg %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L9
	jmp .L10
.L9:
	movq %r12, %r10
	movq $2, %r11
	imulq %r11, %r10
	movq %r10, %r11
	movslq %r11d, %r11
.L11:
	movq $1, %r10
	movq $1, %rbx
	cmpq %rbx, %r10
	sete %r10b
	movzbq %r10b, %r10
	cmpq $0, %r10
	jne .L12
	jmp .L13
.L12:
	leaq -12(%rbp), %r10
	movq %r11, %rbx
	movq $2, %r11
	imulq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L13
.L13:
	jmp .L10
.L10:
	jmp .L7
.L7:
	jmp .L2
.L4:
.L14:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
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
	subq $16, %rsp
	pushq %rbx


.L15:
	movq $2, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	movq %r11, %r10
	movq %r11, %rbx
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
	movq %rbx, %rsi
	movslq %esi, %rsi
	call deepWhileBr
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
	movq %rax, %r10
.Lend17:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
