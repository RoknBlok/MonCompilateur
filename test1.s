			# This code was produced by the CERI Compiler
.data
FormatString1:	.string "%llu"	# used by printf to display 64-bit unsigned integers
FormatString2:	.string "%lf"	# used by printf to display 64-bit floating point numbers
FormatString3:	.string "%c"	# used by printf to display a 8-bit single character
TrueString:	.string "TRUE"	# used by printf to display the boolean value TRUE
FalseString:	.string "FALSE"	# used by printf to display the boolean value FALSE
a:	.quad 0
i:	.quad 0
	.align 8
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $1
	pop i
For1:
TestFor1:
	push $10
	popq	%rax
	cmpq	%rax, i
	ja	EndFor1
	push $10
	pop a
For2:
TestFor2:
	push $1
	popq	%rax
	cmpq	%rax, a
	jb	EndFor2
	push a
	pop %rsi	# The value to be displayed
	movq $FormatString1, %rdi	# "%llu\n"
	movl	$0, %eax
	call	printf@PLT
	movq $0, %rax
	movb $'\n',%al
	push %rax	# push a 64-bit version of '\n'
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
	subq	$1, a
	jmp	TestFor2
EndFor2:
	addq	$1, i
	jmp	TestFor1
EndFor1:
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
