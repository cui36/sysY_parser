.text
.global ifElse
ifElse:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbx


.L1:
	movq $5, %r10
	movq %r10, %r11
	movslq %r11d, %r11
.L2:
	movq %r11, %r10
	movq $5, %rbx
	cmpq %rbx, %r10
	sete %r10b
	movzbq %r10b, %r10
	cmpq $0, %r10
	jne .L3
	jmp .L5
.L3:
	movq $25, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	jmp .L4
.L5:
	movq %r11, %r10
	movq $2, %rbx
	imulq %rbx, %r10
	movq %r10, %r11
	movslq %r11d, %r11
	jmp .L4
.L4:
.L6:
	movq %r11, %r10
	movq %r10, %rax
	jmp .Lend8
.Lend8:
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


.L7:
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
	call ifElse
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
.Lend9:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
