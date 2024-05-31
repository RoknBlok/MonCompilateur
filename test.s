			# This code was produced by the CERI Compiler
.data
FormatString1:	.string "%llu"	# used by printf to display 64-bit unsigned integers
FormatString2:	.string "%lf"	# used by printf to display 64-bit floating point numbers
FormatString3:	.string "%c"	# used by printf to display a 8-bit single character
TrueString:	.string "TRUE"	# used by printf to display the boolean value TRUE
FalseString:	.string "FALSE"	# used by printf to display the boolean value FALSE
a:	.quad 0
b:	.quad 0
c:	.quad 0
	.align 8
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $2
	pop a
	push $0
	pop b
	push $0
	pop c
Case1 :
	push a
CaseListElement1_1:
	push $1
pop %rbx
pop %rax
cmpq %rax, %rbx
je CaseStatement1_1
push %rax
	push $2
pop %rbx
pop %rax
cmpq %rax, %rbx
je CaseStatement1_1
push %rax
jmp CaseListElement1_2
CaseStatement1_1:
	push $100
	pop a
jmp EndCase1
CaseListElement1_2:
	push $6
pop %rbx
pop %rax
cmpq %rax, %rbx
je CaseStatement1_2
push %rax
CaseStatement1_2:
	push $50
	pop a
jmp EndCase1
CaseListElement1_3:
	push b
pop %rbx
pop %rax
cmpq %rax, %rbx
je CaseStatement1_3
push %rax
CaseStatement1_3:
	push $4
	pop a
jmp EndCase1
CaseListElement1_4:
	pop 	%rax
	xor 	%rax, %rax		# rax = 0
	push $8
	pop a
EndCase1:
	push a
	pop %rsi	# The value to be displayed
	movq $FormatString1, %rdi	# "%llu\n"
	movl	$0, %eax
	call	printf@PLT
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
