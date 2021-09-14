.text
.global if_ifElse_
if_ifElse_:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
	movq $5, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	movq $10, %r10
	movq %r10, %rbx
	movslq %ebx, %rbx
.L2:
	movq %r11, %r10
	movq $5, %r12
	cmpq %r12, %r10
	sete %r10b
	movzbq %r10b, %r10
	cmpq $0, %r10
	jne .L3
	jmp .L4
.L3:
.L5:
	movq %rbx, %r10
	movq $10, %rbx
	cmpq %rbx, %r10
	sete %r10b
	movzbq %r10b, %r10
	cmpq $0, %r10
	jne .L6
	jmp .L8
.L6:
	movq $25, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	jmp .L7
.L8:
	movq %r11, %r10
	movq $15, %rbx
	addq %rbx, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	jmp .L7
.L7:
	jmp .L4
.L4:
.L9:
	movq %r11, %r10
	movq %r10, %rax
	jmp .Lend11
.Lend11:
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


.L10:
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
	call if_ifElse_
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
.Lend12:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
