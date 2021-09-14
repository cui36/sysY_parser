.text
.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
	movq $10, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	movq $30, %r10
	movq %r10, %rbx
	movslq %ebx, %rbx
	movq %r11, %r10
	movq $5, %r11
	addq %r11, %r10
	movq %rbx, %r11
	addq %r11, %r10
	movq $5, %r11
	subq %r11, %r10
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
.Lend2:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
