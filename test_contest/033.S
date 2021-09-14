.text
.global get_one
get_one:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
.L2:
	movq $1, %r10
	movq %r10, %rax
	jmp .Lend18
.Lend18:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
.text
.global deepWhileBr
deepWhileBr:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	pushq %rbx


.L3:
	leaq -12(%rbp), %r10
	addq %rsi, %rdi
	movl %edi, (%r10)
	movslq %r10d, %r10
.L4:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $75, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L5
	jmp .L6
.L5:
	movq $42, %rbx
	movq %rbx, %r12
	movslq %r12d, %r12
.L7:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $100, %r10
	cmpq %r10, %r11
	setl %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L8
	jmp .L9
.L8:
	leaq -12(%rbp), %r10
	leaq -12(%rbp), %r11
	movslq (%r11), %rbx
	movq %r12, %r11
	addq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
.L10:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq $99, %r10
	cmpq %r10, %r11
	setg %r11b
	movzbq %r11b, %r11
	cmpq $0, %r11
	jne .L11
	jmp .L12
.L11:
	movq %r12, %r10
	movq $2, %r11
	imulq %r11, %r10
	movq %r10, %r11
	movslq %r11d, %r11
.L13:
	movq $0, %r10
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
	call get_one
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
	movq %rax, %rbx
	movq $1, %r10
	cmpq %r10, %rbx
	sete %bl
	movzbq %bl, %rbx
	cmpq $0, %rbx
	jne .L14
	jmp .L15
.L14:
	leaq -12(%rbp), %r10
	movq %r11, %rbx
	movq $2, %r11
	imulq %r11, %rbx
	movl %ebx, (%r10)
	movslq %r10d, %r10
	jmp .L15
.L15:
	jmp .L12
.L12:
	jmp .L9
.L9:
	jmp .L4
.L6:
.L16:
	leaq -12(%rbp), %r10
	movslq (%r10), %r11
	movq %r11, %rax
	jmp .Lend19
.Lend19:
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


.L17:
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
	movq %rax, %r12
	movq %r12, %r11
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
	movq $0, %r10
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
.Lend20:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
