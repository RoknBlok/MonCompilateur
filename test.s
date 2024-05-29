			# This code was produced by the CERI Compiler
.data
FormatString1:	.string "%llu"	# used by printf to display 64-bit unsigned integers
FormatString2:	.string "%lf"	# used by printf to display 64-bit floating point numbers
FormatString3:	.string "%c"	# used by printf to display a 8-bit single character
TrueString:	.string "TRUE"	# used by printf to display the boolean value TRUE
FalseString:	.string "FALSE"	# used by printf to display the boolean value FALSE
a:	.quad 0
b:	.quad 0
c1:	.byte 0
c2:	.byte 0
denum:	.double 0.0
frac:	.double 0.0
num:	.double 0.0
	.align 8
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	movq $0, %rax
	movb $'f',%al
	push %rax	# push a 64-bit version of 'f'
	pop %rax
	movb %al,c1
	movq $0, %rax
	movb $'a',%al
	push %rax	# push a 64-bit version of 'a'
	pop %rax
	movb %al,c2
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$0, (%rsp)	# Conversion of 1 (32 bit high part)
	movl	$1072693248, 4(%rsp)	# Conversion of 1 (32 bit low part)
	pop num
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$0, (%rsp)	# Conversion of 1 (32 bit high part)
	movl	$1072693248, 4(%rsp)	# Conversion of 1 (32 bit low part)
	pop denum
	push num
	push denum
	fldl	(%rsp)	
	fldl	8(%rsp)	# first operand -> %st(0) ; second operand -> %st(1)
	fdivp	%st(0),%st(1)	# %st(0) <- op1 + op2 ; %st(1)=null
	fstpl 8(%rsp)
	addq	$8, %rsp	# result on stack's top
	pop frac
	push $1
	pop a
While0:
	push frac
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$2576980378, (%rsp)	# Conversion of 0.1 (32 bit high part)
	movl	$1069128089, 4(%rsp)	# Conversion of 0.1 (32 bit low part)
	fldl	(%rsp)	
	fldl	8(%rsp)	# first operand -> %st(0) ; second operand -> %st(1)
	 addq $16, %rsp	# 2x pop nothing
	fcomip %st(1)		# compare op1 and op2 -> %RFLAGS and pop
	faddp %st(1)	# pop nothing
	ja Vrai2	# If above
	push $0		# False
	jmp Suite2
Vrai2:	push $0xFFFFFFFFFFFFFFFF		# True
Suite2:
	pop %rax	# Get the result of expression
	cmpq $0, %rax
	je EndWhile0	# if FALSE, jump out of the loop0
	push c1
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
	movq $0, %rax
	movb $'=',%al
	push %rax	# push a 64-bit version of '='
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
	push frac
	movsd	(%rsp), %xmm0		# &stack top -> %xmm0
	subq	$16, %rsp		# allocation for 3 additional doubles
	movsd %xmm0, 8(%rsp)
	movq $FormatString2, %rdi	# "%lf\n"
	movq	$1, %rax
	call	printf
nop
	addq $24, %rsp			# pop nothing
	movq $0, %rax
	movb $'\n',%al
	push %rax	# push a 64-bit version of '\n'
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
	push c2
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
	movq $0, %rax
	movb $'=',%al
	push %rax	# push a 64-bit version of '='
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
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
	push denum
	subq $8,%rsp			# allocate 8 bytes on stack's top
	movl	$0, (%rsp)	# Conversion of 1 (32 bit high part)
	movl	$1072693248, 4(%rsp)	# Conversion of 1 (32 bit low part)
	fldl	8(%rsp)	
	fldl	(%rsp)	# first operand -> %st(0) ; second operand -> %st(1)
	faddp	%st(0),%st(1)	# %st(0) <- op1 + op2 ; %st(1)=null
	fstpl 8(%rsp)
	addq	$8, %rsp	# result on stack's top
	pop denum
	push num
	push denum
	fldl	(%rsp)	
	fldl	8(%rsp)	# first operand -> %st(0) ; second operand -> %st(1)
	fdivp	%st(0),%st(1)	# %st(0) <- op1 + op2 ; %st(1)=null
	fstpl 8(%rsp)
	addq	$8, %rsp	# result on stack's top
	pop frac
	push a
	push $1
	pop %rbx
	pop %rax
	addq	%rbx, %rax	# ADD
	push %rax
	pop a
	push a
	push $3
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	ja Vrai12	# If above
	push $0		# False
	jmp Suite12
Vrai12:	push $0xFFFFFFFFFFFFFFFF		# True
Suite12:
	pop %rdx	# Zero : False, non-zero : true
	cmpq $0, %rdx
	je False11
	movq $TrueString, %rdi	# "TRUE\n"
	jmp Next11
False11:
	movq $FalseString, %rdi	# "FALSE\n"
Next11:
	call	puts@PLT
	movq $0, %rax
	movb $'\n',%al
	push %rax	# push a 64-bit version of '\n'
	pop %rsi			# get character in the 8 lowest bits of %si
	movq $FormatString3, %rdi	# "%c\n"
	movl	$0, %eax
	call	printf@PLT
	jmp While0
EndWhile0:
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
