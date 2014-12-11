	.file	"StrIO.mod"
	.text
.Ltext0:
	.globl	_M2_StrIO_init
	.type	_M2_StrIO_init, @function
_M2_StrIO_init:
.LFB0:
	.file 1 "../../../gm2-libs/StrIO.mod"
	.loc 1 194 0
	.cfi_startproc
	pushq	%rbp
.LCFI0:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI1:
	.cfi_def_cfa_register 6
	.loc 1 197 0
	movl	$0, IsATTY(%rip)
	.loc 1 194 0
	popq	%rbp
.LCFI2:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	_M2_StrIO_init, .-_M2_StrIO_init
	.globl	_M2_StrIO_finish
	.type	_M2_StrIO_finish, @function
_M2_StrIO_finish:
.LFB1:
	.loc 1 197 0
	.cfi_startproc
	pushq	%rbp
.LCFI3:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI4:
	.cfi_def_cfa_register 6
	.loc 1 197 0
	popq	%rbp
.LCFI5:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	_M2_StrIO_finish, .-_M2_StrIO_finish
	.globl	StrIO_WriteLn
	.type	StrIO_WriteLn, @function
StrIO_WriteLn:
.LFB2:
	.loc 1 47 0
	.cfi_startproc
	pushq	%rbp
.LCFI6:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI7:
	.cfi_def_cfa_register 6
	.loc 1 49 0
	movl	$13, %edi
	call	Echo
	.loc 1 51 0
	movl	$10, %edi
	call	StdIO_Write@PLT
	popq	%rbp
.LCFI8:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	StrIO_WriteLn, .-StrIO_WriteLn
	.globl	StrIO_ReadString
	.type	StrIO_ReadString, @function
StrIO_ReadString:
.LFB3:
	.loc 1 64 0
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA3
	pushq	%rbp
.LCFI9:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI10:
	.cfi_def_cfa_register 6
.LEHB0:
	subq	$224, %rsp
.LEHE0:
	movq	%rdi, %rdx
	movl	%esi, %eax
	movq	%rdx, -224(%rbp)
	movl	%eax, -216(%rbp)
	.loc 1 66 0
	movl	-216(%rbp), %eax
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %eax
	movl	%eax, -12(%rbp)
	.loc 1 67 0
	movl	$0, -4(%rbp)
.L5:
	.loc 1 69 0
	leaq	-201(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
.LEHB1:
	call	StdIO_Read@PLT
	.loc 1 70 0
	movzbl	-201(%rbp), %eax
	cmpb	$127, %al
	je	.L7
.L6:
	.loc 1 70 0 is_stmt 0 discriminator 2
	movzbl	-201(%rbp), %eax
	cmpb	$8, %al
	jne	.L41
.L7:
	.loc 1 73 0 is_stmt 1
	cmpl	$0, -4(%rbp)
	jne	.L42
.L9:
	.loc 1 75 0
	movl	$7, %edi
	call	StdIO_Write@PLT
	.loc 1 76 0
	jmp	.L11
.L42:
.L10:
	call	Erase
	.loc 1 78 0
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -200(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -4(%rbp)
	.loc 1 79 0
	jmp	.L11
.L41:
.L8:
	.loc 1 80 0
	movzbl	-201(%rbp), %eax
	cmpb	$21, %al
	jne	.L43
.L12:
	.loc 1 81 0
	movl	-4(%rbp), %eax
	testq	%rax, %rax
	jle	.L11
.L14:
	.loc 1 82 0
	call	Erase
	.loc 1 84 0
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -196(%rbp)
	movl	-196(%rbp), %eax
	movl	%eax, -4(%rbp)
	.loc 1 85 0
	jmp	.L12
.L43:
.L13:
	.loc 1 86 0
	movzbl	-201(%rbp), %eax
	cmpb	$23, %al
	jne	.L44
.L15:
	.loc 1 88 0
	cmpl	$0, -4(%rbp)
	jne	.L45
.L17:
	.loc 1 90 0
	movl	$7, %edi
	call	Echo
	jmp	.L11
.L45:
.L18:
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -128(%rbp)
	movl	-128(%rbp), %eax
	movl	%eax, -132(%rbp)
	movl	-132(%rbp), %eax
	movl	%eax, -136(%rbp)
	movq	-224(%rbp), %rdx
	movl	-136(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -144(%rbp)
	.loc 1 91 0
	movq	-144(%rbp), %rax
	movzbl	(%rax), %eax
	.loc 1 91 0 is_stmt 0 discriminator 1
	movb	%al, -145(%rbp)
	movzbl	-145(%rbp), %eax
	movl	%eax, %edi
	call	AlphaNum
	movl	%eax, -152(%rbp)
	.loc 1 92 0 is_stmt 1 discriminator 1
	cmpl	$1, -152(%rbp)
	jne	.L46
.L19:
	.loc 1 93 0
	call	Erase
	.loc 1 95 0
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -160(%rbp)
	movl	-160(%rbp), %eax
	movl	%eax, -4(%rbp)
	cmpl	$0, -4(%rbp)
	je	.L11
.L21:
	.loc 1 95 0 is_stmt 0 discriminator 2
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -164(%rbp)
	movl	-164(%rbp), %eax
	movl	%eax, -168(%rbp)
	movl	-168(%rbp), %eax
	movl	%eax, -172(%rbp)
	movq	-224(%rbp), %rdx
	movl	-172(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rax
	movzbl	(%rax), %eax
	.loc 1 95 0 discriminator 3
	movb	%al, -185(%rbp)
	movzbl	-185(%rbp), %eax
	movl	%eax, %edi
	call	AlphaNum
	movl	%eax, -192(%rbp)
	cmpl	$1, -192(%rbp)
	jne	.L51
	.loc 1 95 0
	jmp	.L19
.L46:
.L20:
	.loc 1 97 0 is_stmt 1
	call	Erase
	.loc 1 99 0
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -156(%rbp)
	movl	-156(%rbp), %eax
	movl	%eax, -4(%rbp)
	.loc 1 100 0
	jmp	.L11
.L44:
.L16:
	.loc 1 101 0
	movl	-4(%rbp), %eax
	cmpl	-12(%rbp), %eax
	ja	.L47
.L23:
	.loc 1 102 0
	movzbl	-201(%rbp), %eax
	cmpb	$13, %al
	je	.L26
.L25:
	.loc 1 102 0 is_stmt 0 discriminator 2
	movzbl	-201(%rbp), %eax
	cmpb	$10, %al
	jne	.L48
.L26:
	.loc 1 104 0 is_stmt 1
	movl	-4(%rbp), %eax
	movl	%eax, -108(%rbp)
	movl	-108(%rbp), %eax
	movl	%eax, -112(%rbp)
	movq	-224(%rbp), %rdx
	movl	-112(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	movb	$0, (%rax)
	.loc 1 106 0
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -124(%rbp)
	movl	-124(%rbp), %eax
	movl	%eax, -4(%rbp)
	jmp	.L11
.L48:
.L27:
	.loc 1 107 0
	movzbl	-201(%rbp), %eax
	cmpb	$12, %al
	jne	.L49
.L28:
	.loc 1 108 0
	movq	-224(%rbp), %rax
	movq	%rax, -96(%rbp)
	movzbl	-201(%rbp), %edx
	movq	-96(%rbp), %rax
	movb	%dl, (%rax)
	.loc 1 110 0
	movl	-12(%rbp), %eax
	testq	%rax, %rax
	jle	.L31
.L30:
	.loc 1 111 0
	movq	-224(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -104(%rbp)
	.loc 1 112 0
	movq	-104(%rbp), %rax
	movb	$0, (%rax)
.L31:
	.loc 1 114 0
	movb	$13, -201(%rbp)
	jmp	.L11
.L49:
.L29:
	.loc 1 115 0
	movzbl	-201(%rbp), %eax
	cmpb	$31, %al
	jbe	.L50
.L32:
	.loc 1 116 0
	movzbl	-201(%rbp), %eax
	movl	%eax, %edi
	call	Echo
	.loc 1 117 0
	movl	-4(%rbp), %eax
	movl	%eax, -68(%rbp)
	movl	-68(%rbp), %eax
	movl	%eax, -72(%rbp)
	movq	-224(%rbp), %rdx
	movl	-72(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -80(%rbp)
	movzbl	-201(%rbp), %edx
	movq	-80(%rbp), %rax
	movb	%dl, (%rax)
	.loc 1 119 0
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -84(%rbp)
	movl	-84(%rbp), %eax
	movl	%eax, -4(%rbp)
	jmp	.L11
.L50:
.L33:
	.loc 1 120 0
	movzbl	-201(%rbp), %eax
	cmpb	$4, %al
	jne	.L11
.L34:
	.loc 1 121 0
	movl	-4(%rbp), %eax
	movl	%eax, -28(%rbp)
	movl	-28(%rbp), %eax
	movl	%eax, -32(%rbp)
	movq	-224(%rbp), %rdx
	movl	-32(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -40(%rbp)
	movzbl	-201(%rbp), %edx
	movq	-40(%rbp), %rax
	movb	%dl, (%rax)
	.loc 1 122 0
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -44(%rbp)
	movl	-44(%rbp), %eax
	movl	%eax, -4(%rbp)
	.loc 1 123 0
	movb	$13, -201(%rbp)
	.loc 1 125 0
	movl	-4(%rbp), %eax
	cmpl	-12(%rbp), %eax
	ja	.L11
.L35:
	.loc 1 126 0
	movl	-4(%rbp), %eax
	movl	%eax, -48(%rbp)
	movl	-48(%rbp), %eax
	movl	%eax, -52(%rbp)
	movq	-224(%rbp), %rdx
	movl	-52(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -64(%rbp)
	.loc 1 127 0
	movq	-64(%rbp), %rax
	movb	$0, (%rax)
	.loc 1 129 0
	jmp	.L11
.L47:
.L24:
	.loc 1 130 0
	movzbl	-201(%rbp), %eax
	cmpb	$13, %al
	je	.L11
.L36:
	.loc 1 132 0
	movl	$7, %edi
	call	Echo
.LEHE1:
	jmp	.L11
.L51:
	.loc 1 97 0
	nop
.L11:
	.loc 1 133 0
	movzbl	-201(%rbp), %eax
	cmpb	$13, %al
	je	.L4
.L37:
	.loc 1 133 0 is_stmt 0 discriminator 2
	movzbl	-201(%rbp), %eax
	cmpb	$10, %al
	je	.L4
	.loc 1 133 0
	jmp	.L5
.L38:
.L40:
	movq	%rax, %rdi
.LEHB2:
	call	_Unwind_Resume@PLT
.L4:
	.loc 1 134 0 is_stmt 1
	leave
.LCFI11:
	.cfi_def_cfa 7, 8
.LEHE2:
	ret
	.cfi_endproc
.LFE3:
	.globl	__gxx_personality_v0
	.section	.gcc_except_table,"a",@progbits
.LLSDA3:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE3-.LLSDACSB3
.LLSDACSB3:
	.uleb128 .LEHB0-.LFB3
	.uleb128 .LEHE0-.LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB1-.LFB3
	.uleb128 .LEHE1-.LEHB1
	.uleb128 .L40-.LFB3
	.uleb128 0
	.uleb128 .LEHB2-.LFB3
	.uleb128 .LEHE2-.LEHB2
	.uleb128 0
	.uleb128 0
.LLSDACSE3:
	.text
	.size	StrIO_ReadString, .-StrIO_ReadString
	.globl	StrIO_WriteString
	.type	StrIO_WriteString, @function
StrIO_WriteString:
.LFB4:
	.loc 1 144 0
	.cfi_startproc
	pushq	%rbp
.LCFI12:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI13:
	.cfi_def_cfa_register 6
	subq	$96, %rsp
	movq	%rdi, %rdx
	movl	%esi, %eax
	movq	%rdx, -80(%rbp)
	movl	%eax, -72(%rbp)
	.loc 1 145 0
	movl	-72(%rbp), %eax
	leal	1(%rax), %esi
	movq	-80(%rbp), %rcx
	movl	-72(%rbp), %eax
	addl	$1, %eax
	movl	%eax, %eax
	leaq	15(%rax), %rdx
	movl	$16, %eax
	subq	$1, %rax
	addq	%rdx, %rax
	movq	$16, -88(%rbp)
	movl	$0, %edx
	divq	-88(%rbp)
	imulq	$16, %rax, %rax
	subq	%rax, %rsp
	movq	%rsp, %rax
	addq	$15, %rax
	shrq	$4, %rax
	salq	$4, %rax
	movl	%esi, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	memcpy@PLT
	movq	%rax, -80(%rbp)
	.loc 1 146 0
	movl	-72(%rbp), %eax
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %eax
	movl	%eax, -12(%rbp)
	.loc 1 147 0
	movl	$0, -4(%rbp)
.L53:
	.loc 1 148 0
	movl	-4(%rbp), %eax
	cmpl	-12(%rbp), %eax
	ja	.L52
.L54:
	.loc 1 148 0 is_stmt 0 discriminator 2
	movl	-4(%rbp), %eax
	movl	%eax, -16(%rbp)
	movl	-16(%rbp), %eax
	movl	%eax, -20(%rbp)
	movq	-80(%rbp), %rdx
	movl	-20(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movzbl	(%rax), %eax
	movb	%al, -33(%rbp)
	cmpb	$0, -33(%rbp)
	je	.L52
.L56:
	.loc 1 149 0 is_stmt 1
	movl	-4(%rbp), %eax
	movl	%eax, -40(%rbp)
	movl	-40(%rbp), %eax
	movl	%eax, -44(%rbp)
	movq	-80(%rbp), %rdx
	movl	-44(%rbp), %eax
	addq	%rdx, %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movzbl	(%rax), %eax
	movb	%al, -57(%rbp)
	movzbl	-57(%rbp), %eax
	movl	%eax, %edi
	call	StdIO_Write@PLT
	.loc 1 151 0
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -4(%rbp)
	.loc 1 152 0
	jmp	.L53
.L55:
.L52:
	leave
.LCFI14:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	StrIO_WriteString, .-StrIO_WriteString
	.type	Erase, @function
Erase:
.LFB5:
	.loc 1 160 0
	.cfi_startproc
	pushq	%rbp
.LCFI15:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI16:
	.cfi_def_cfa_register 6
	.loc 1 162 0
	movl	$8, %edi
	call	Echo
	.loc 1 163 0
	movl	$32, %edi
	call	Echo
	.loc 1 165 0
	movl	$8, %edi
	call	Echo
	popq	%rbp
.LCFI17:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	Erase, .-Erase
	.type	Echo, @function
Echo:
.LFB6:
	.loc 1 173 0
	.cfi_startproc
	pushq	%rbp
.LCFI18:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI19:
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movb	%dil, -1(%rbp)
	.loc 1 177 0
	movl	IsATTY(%rip), %eax
	cmpl	$1, %eax
	jne	.L58
.L59:
	.loc 1 178 0
	movzbl	-1(%rbp), %eax
	movl	%eax, %edi
	call	StdIO_Write@PLT
.L60:
.L58:
	.loc 1 179 0
	leave
.LCFI20:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	Echo, .-Echo
	.type	AlphaNum, @function
AlphaNum:
.LFB7:
	.loc 1 186 0
	.cfi_startproc
	pushq	%rbp
.LCFI21:
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
.LCFI22:
	.cfi_def_cfa_register 6
	movb	%dil, -17(%rbp)
	.loc 1 188 0
	cmpb	$96, -17(%rbp)
	jbe	.L63
.L62:
	.loc 1 188 0 is_stmt 0 discriminator 2
	cmpb	$122, -17(%rbp)
	jbe	.L64
.L63:
	.loc 1 189 0 is_stmt 1
	cmpb	$64, -17(%rbp)
	jbe	.L66
.L65:
	.loc 1 189 0 is_stmt 0 discriminator 2
	cmpb	$90, -17(%rbp)
	jbe	.L64
.L66:
	.loc 1 190 0 is_stmt 1
	cmpb	$47, -17(%rbp)
	jbe	.L68
.L67:
	.loc 1 190 0 is_stmt 0 discriminator 2
	cmpb	$57, -17(%rbp)
	ja	.L68
.L64:
	.loc 1 191 0 is_stmt 1
	movl	$1, -4(%rbp)
	jmp	.L69
.L68:
	movl	$0, -4(%rbp)
.L69:
	.loc 1 191 0 is_stmt 0 discriminator 1
	movl	-4(%rbp), %eax
	popq	%rbp
.LCFI23:
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	AlphaNum, .-AlphaNum
	.local	IsATTY
	.comm	IsATTY,4,4
.Letext0:
	.file 2 "../../../gm2-libs/M2RTS.def"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.long	0x6ef
	.value	0x2
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.LASF115
	.byte	0x1
	.long	.LASF116
	.long	.LASF117
	.quad	.Ltext0
	.quad	.Letext0
	.long	.Ldebug_line0
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.long	.LASF0
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.long	.LASF1
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.long	.LASF2
	.uleb128 0x3
	.byte	0x1
	.byte	0x7
	.string	"LOC"
	.uleb128 0x2
	.byte	0x1
	.byte	0x8
	.long	.LASF3
	.uleb128 0x4
	.byte	0x8
	.long	.LASF118
	.uleb128 0x2
	.byte	0x8
	.byte	0x5
	.long	.LASF4
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.long	.LASF5
	.uleb128 0x2
	.byte	0x2
	.byte	0x5
	.long	.LASF6
	.uleb128 0x2
	.byte	0x2
	.byte	0x7
	.long	.LASF7
	.uleb128 0x2
	.byte	0x4
	.byte	0x4
	.long	.LASF8
	.uleb128 0x2
	.byte	0x8
	.byte	0x4
	.long	.LASF9
	.uleb128 0x2
	.byte	0x10
	.byte	0x4
	.long	.LASF10
	.uleb128 0x2
	.byte	0x10
	.byte	0x3
	.long	.LASF11
	.uleb128 0x2
	.byte	0x20
	.byte	0x3
	.long	.LASF12
	.uleb128 0x2
	.byte	0x8
	.byte	0x3
	.long	.LASF13
	.uleb128 0x2
	.byte	0x1
	.byte	0x5
	.long	.LASF14
	.uleb128 0x2
	.byte	0x2
	.byte	0x5
	.long	.LASF15
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.long	.LASF16
	.uleb128 0x2
	.byte	0x8
	.byte	0x5
	.long	.LASF17
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.long	.LASF18
	.uleb128 0x2
	.byte	0x2
	.byte	0x7
	.long	.LASF19
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.long	.LASF20
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.long	.LASF21
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.long	.LASF22
	.uleb128 0x2
	.byte	0x2
	.byte	0x7
	.long	.LASF23
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.long	.LASF24
	.uleb128 0x2
	.byte	0x4
	.byte	0x4
	.long	.LASF25
	.uleb128 0x2
	.byte	0x8
	.byte	0x4
	.long	.LASF26
	.uleb128 0x2
	.byte	0
	.byte	0x4
	.long	.LASF27
	.uleb128 0x2
	.byte	0x10
	.byte	0x4
	.long	.LASF28
	.uleb128 0x2
	.byte	0x8
	.byte	0x3
	.long	.LASF29
	.uleb128 0x2
	.byte	0x10
	.byte	0x3
	.long	.LASF30
	.uleb128 0x2
	.byte	0
	.byte	0x3
	.long	.LASF31
	.uleb128 0x2
	.byte	0x20
	.byte	0x3
	.long	.LASF32
	.uleb128 0x5
	.byte	0x10
	.byte	0x2
	.byte	0x6d
	.long	0x146
	.uleb128 0x6
	.long	.LASF33
	.byte	0x2
	.byte	0x6d
	.long	0x146
	.byte	0x2
	.byte	0x23
	.uleb128 0
	.uleb128 0x6
	.long	.LASF34
	.byte	0x2
	.byte	0x6d
	.long	0x2d
	.byte	0x2
	.byte	0x23
	.uleb128 0x8
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x3b
	.uleb128 0x8
	.byte	0x1
	.long	.LASF35
	.byte	0x1
	.byte	0xc2
	.long	0x34
	.quad	.LFB0
	.quad	.LFE0
	.long	.LLST0
	.byte	0x1
	.uleb128 0x8
	.byte	0x1
	.long	.LASF36
	.byte	0x1
	.byte	0xc5
	.long	0x34
	.quad	.LFB1
	.quad	.LFE1
	.long	.LLST1
	.byte	0x1
	.uleb128 0x9
	.byte	0x1
	.long	.LASF37
	.byte	0x1
	.byte	0x2f
	.byte	0x1
	.quad	.LFB2
	.quad	.LFE2
	.long	.LLST2
	.byte	0x1
	.uleb128 0xa
	.byte	0x1
	.long	.LASF92
	.byte	0x1
	.byte	0x40
	.byte	0x1
	.quad	.LFB3
	.quad	.LFE3
	.long	.LLST3
	.byte	0x1
	.long	0x518
	.uleb128 0xb
	.string	"a"
	.byte	0x1
	.byte	0x3c
	.long	0x121
	.byte	0x3
	.byte	0x91
	.sleb128 -240
	.uleb128 0xc
	.long	.LASF38
	.byte	0x1
	.byte	0x7e
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0xc
	.long	.LASF39
	.byte	0x1
	.byte	0x7e
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -68
	.uleb128 0xc
	.long	.LASF40
	.byte	0x1
	.byte	0x7e
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0xc
	.long	.LASF41
	.byte	0x1
	.byte	0x7a
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -60
	.uleb128 0xc
	.long	.LASF42
	.byte	0x1
	.byte	0x79
	.long	0x146
	.byte	0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0xc
	.long	.LASF43
	.byte	0x1
	.byte	0x79
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0xc
	.long	.LASF44
	.byte	0x1
	.byte	0x79
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -44
	.uleb128 0xc
	.long	.LASF45
	.byte	0x1
	.byte	0x77
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -100
	.uleb128 0xc
	.long	.LASF46
	.byte	0x1
	.byte	0x75
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -96
	.uleb128 0xc
	.long	.LASF47
	.byte	0x1
	.byte	0x75
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0xc
	.long	.LASF48
	.byte	0x1
	.byte	0x75
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -84
	.uleb128 0xc
	.long	.LASF49
	.byte	0x1
	.byte	0x6f
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -120
	.uleb128 0xc
	.long	.LASF50
	.byte	0x1
	.byte	0x6c
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -112
	.uleb128 0xc
	.long	.LASF51
	.byte	0x1
	.byte	0x6a
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -140
	.uleb128 0xc
	.long	.LASF52
	.byte	0x1
	.byte	0x68
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0xc
	.long	.LASF53
	.byte	0x1
	.byte	0x68
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -128
	.uleb128 0xc
	.long	.LASF54
	.byte	0x1
	.byte	0x68
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -124
	.uleb128 0xc
	.long	.LASF55
	.byte	0x1
	.byte	0x63
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -172
	.uleb128 0xc
	.long	.LASF56
	.byte	0x1
	.byte	0x5f
	.long	0x34
	.byte	0x3
	.byte	0x91
	.sleb128 -208
	.uleb128 0xc
	.long	.LASF57
	.byte	0x1
	.byte	0x5f
	.long	0x3b
	.byte	0x3
	.byte	0x91
	.sleb128 -201
	.uleb128 0xc
	.long	.LASF58
	.byte	0x1
	.byte	0x5f
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -200
	.uleb128 0xc
	.long	.LASF59
	.byte	0x1
	.byte	0x5f
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -188
	.uleb128 0xc
	.long	.LASF60
	.byte	0x1
	.byte	0x5f
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -184
	.uleb128 0xc
	.long	.LASF61
	.byte	0x1
	.byte	0x5f
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -180
	.uleb128 0xc
	.long	.LASF62
	.byte	0x1
	.byte	0x5f
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -176
	.uleb128 0xc
	.long	.LASF63
	.byte	0x1
	.byte	0x5b
	.long	0x34
	.byte	0x3
	.byte	0x91
	.sleb128 -168
	.uleb128 0xc
	.long	.LASF64
	.byte	0x1
	.byte	0x5b
	.long	0x3b
	.byte	0x3
	.byte	0x91
	.sleb128 -161
	.uleb128 0xc
	.long	.LASF65
	.byte	0x1
	.byte	0x5a
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0xc
	.long	.LASF66
	.byte	0x1
	.byte	0x5a
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -152
	.uleb128 0xc
	.long	.LASF67
	.byte	0x1
	.byte	0x5a
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -148
	.uleb128 0xc
	.long	.LASF68
	.byte	0x1
	.byte	0x5a
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -144
	.uleb128 0xc
	.long	.LASF69
	.byte	0x1
	.byte	0x54
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -212
	.uleb128 0xc
	.long	.LASF70
	.byte	0x1
	.byte	0x4e
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -216
	.uleb128 0xc
	.long	.LASF71
	.byte	0x1
	.byte	0x45
	.long	0x50
	.byte	0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0xc
	.long	.LASF72
	.byte	0x1
	.byte	0x42
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0xd
	.string	"ch"
	.byte	0x1
	.byte	0x40
	.long	0x3b
	.byte	0x3
	.byte	0x91
	.sleb128 -217
	.uleb128 0xc
	.long	.LASF73
	.byte	0x1
	.byte	0x3f
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0xd
	.string	"n"
	.byte	0x1
	.byte	0x3f
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -20
	.uleb128 0xe
	.long	.LASF74
	.byte	0x1
	.byte	0x85
	.quad	.L38
	.uleb128 0xe
	.long	.LASF75
	.byte	0x1
	.byte	0x73
	.quad	.L33
	.uleb128 0xe
	.long	.LASF76
	.byte	0x1
	.byte	0x6e
	.quad	.L31
	.uleb128 0xe
	.long	.LASF77
	.byte	0x1
	.byte	0x6b
	.quad	.L29
	.uleb128 0xe
	.long	.LASF78
	.byte	0x1
	.byte	0x66
	.quad	.L27
	.uleb128 0xe
	.long	.LASF79
	.byte	0x1
	.byte	0x66
	.quad	.L26
	.uleb128 0xe
	.long	.LASF80
	.byte	0x1
	.byte	0x65
	.quad	.L24
	.uleb128 0xe
	.long	.LASF81
	.byte	0x1
	.byte	0x5d
	.quad	.L19
	.uleb128 0xe
	.long	.LASF82
	.byte	0x1
	.byte	0x5c
	.quad	.L20
	.uleb128 0xe
	.long	.LASF83
	.byte	0x1
	.byte	0x58
	.quad	.L18
	.uleb128 0xe
	.long	.LASF84
	.byte	0x1
	.byte	0x56
	.quad	.L16
	.uleb128 0xe
	.long	.LASF85
	.byte	0x1
	.byte	0x51
	.quad	.L12
	.uleb128 0xe
	.long	.LASF86
	.byte	0x1
	.byte	0x50
	.quad	.L13
	.uleb128 0xe
	.long	.LASF87
	.byte	0x1
	.byte	0x4c
	.quad	.L11
	.uleb128 0xe
	.long	.LASF88
	.byte	0x1
	.byte	0x49
	.quad	.L10
	.uleb128 0xe
	.long	.LASF89
	.byte	0x1
	.byte	0x46
	.quad	.L8
	.uleb128 0xe
	.long	.LASF90
	.byte	0x1
	.byte	0x46
	.quad	.L7
	.uleb128 0xe
	.long	.LASF91
	.byte	0x1
	.byte	0x45
	.quad	.L5
	.byte	0
	.uleb128 0xa
	.byte	0x1
	.long	.LASF93
	.byte	0x1
	.byte	0x90
	.byte	0x1
	.quad	.LFB4
	.quad	.LFE4
	.long	.LLST4
	.byte	0x1
	.long	0x60f
	.uleb128 0xb
	.string	"a"
	.byte	0x1
	.byte	0x8d
	.long	0x121
	.byte	0x3
	.byte	0x91
	.sleb128 -96
	.uleb128 0xc
	.long	.LASF94
	.byte	0x1
	.byte	0x97
	.long	0x2d
	.byte	0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0xc
	.long	.LASF95
	.byte	0x1
	.byte	0x95
	.long	0x3b
	.byte	0x3
	.byte	0x91
	.sleb128 -73
	.uleb128 0xc
	.long	.LASF96
	.byte	0x1
	.byte	0x95
	.long	0x146
	.byte	0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0xc
	.long	.LASF97
	.byte	0x1
	.byte	0x95
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -60
	.uleb128 0xc
	.long	.LASF98
	.byte	0x1
	.byte	0x95
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0xc
	.long	.LASF99
	.byte	0x1
	.byte	0x94
	.long	0x3b
	.byte	0x2
	.byte	0x91
	.sleb128 -49
	.uleb128 0xc
	.long	.LASF100
	.byte	0x1
	.byte	0x94
	.long	0x146
	.byte	0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0xc
	.long	.LASF101
	.byte	0x1
	.byte	0x94
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -36
	.uleb128 0xc
	.long	.LASF102
	.byte	0x1
	.byte	0x94
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0xc
	.long	.LASF103
	.byte	0x1
	.byte	0x92
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0xc
	.long	.LASF73
	.byte	0x1
	.byte	0x90
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0xd
	.string	"n"
	.byte	0x1
	.byte	0x90
	.long	0x2d
	.byte	0x2
	.byte	0x91
	.sleb128 -20
	.uleb128 0xe
	.long	.LASF104
	.byte	0x1
	.byte	0x94
	.quad	.L55
	.uleb128 0xe
	.long	.LASF105
	.byte	0x1
	.byte	0x94
	.quad	.L53
	.byte	0
	.uleb128 0xf
	.long	.LASF119
	.byte	0x1
	.byte	0xa0
	.byte	0x1
	.quad	.LFB5
	.quad	.LFE5
	.long	.LLST5
	.byte	0x1
	.uleb128 0x10
	.long	.LASF107
	.byte	0x1
	.byte	0xad
	.byte	0x1
	.quad	.LFB6
	.quad	.LFE6
	.long	.LLST6
	.byte	0x1
	.long	0x66a
	.uleb128 0xb
	.string	"ch"
	.byte	0x1
	.byte	0xad
	.long	0x3b
	.byte	0x2
	.byte	0x91
	.sleb128 -17
	.uleb128 0xe
	.long	.LASF106
	.byte	0x1
	.byte	0xb1
	.quad	.L60
	.byte	0
	.uleb128 0x11
	.long	.LASF108
	.byte	0x1
	.byte	0xba
	.byte	0x1
	.long	0x34
	.quad	.LFB7
	.quad	.LFE7
	.long	.LLST7
	.byte	0x1
	.uleb128 0xb
	.string	"ch"
	.byte	0x1
	.byte	0xba
	.long	0x3b
	.byte	0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0xc
	.long	.LASF109
	.byte	0x1
	.byte	0xbf
	.long	0x34
	.byte	0x2
	.byte	0x91
	.sleb128 -20
	.uleb128 0xe
	.long	.LASF110
	.byte	0x1
	.byte	0xbf
	.quad	.L69
	.uleb128 0xe
	.long	.LASF111
	.byte	0x1
	.byte	0xbe
	.quad	.L68
	.uleb128 0xe
	.long	.LASF112
	.byte	0x1
	.byte	0xbd
	.quad	.L66
	.uleb128 0xe
	.long	.LASF113
	.byte	0x1
	.byte	0xbc
	.quad	.L64
	.uleb128 0xe
	.long	.LASF114
	.byte	0x1
	.byte	0xbc
	.quad	.L63
	.byte	0
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x10
	.uleb128 0x6
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x13
	.byte	0x1
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x2117
	.uleb128 0xc
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x2116
	.uleb128 0xc
	.byte	0
	.byte	0
	.uleb128 0xa
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x2116
	.uleb128 0xc
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xb
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0xc
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0xd
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0xe
	.uleb128 0xa
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.byte	0
	.byte	0
	.uleb128 0xf
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x2116
	.uleb128 0xc
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x2116
	.uleb128 0xc
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x11
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x2117
	.uleb128 0xc
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
.LLST0:
	.quad	.LFB0-.Ltext0
	.quad	.LCFI0-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI0-.Ltext0
	.quad	.LCFI1-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI1-.Ltext0
	.quad	.LCFI2-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI2-.Ltext0
	.quad	.LFE0-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST1:
	.quad	.LFB1-.Ltext0
	.quad	.LCFI3-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI3-.Ltext0
	.quad	.LCFI4-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI4-.Ltext0
	.quad	.LCFI5-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI5-.Ltext0
	.quad	.LFE1-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST2:
	.quad	.LFB2-.Ltext0
	.quad	.LCFI6-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI6-.Ltext0
	.quad	.LCFI7-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI7-.Ltext0
	.quad	.LCFI8-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI8-.Ltext0
	.quad	.LFE2-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST3:
	.quad	.LFB3-.Ltext0
	.quad	.LCFI9-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI9-.Ltext0
	.quad	.LCFI10-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI10-.Ltext0
	.quad	.LCFI11-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI11-.Ltext0
	.quad	.LFE3-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST4:
	.quad	.LFB4-.Ltext0
	.quad	.LCFI12-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI12-.Ltext0
	.quad	.LCFI13-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI13-.Ltext0
	.quad	.LCFI14-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI14-.Ltext0
	.quad	.LFE4-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST5:
	.quad	.LFB5-.Ltext0
	.quad	.LCFI15-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI15-.Ltext0
	.quad	.LCFI16-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI16-.Ltext0
	.quad	.LCFI17-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI17-.Ltext0
	.quad	.LFE5-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST6:
	.quad	.LFB6-.Ltext0
	.quad	.LCFI18-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI18-.Ltext0
	.quad	.LCFI19-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI19-.Ltext0
	.quad	.LCFI20-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI20-.Ltext0
	.quad	.LFE6-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
.LLST7:
	.quad	.LFB7-.Ltext0
	.quad	.LCFI21-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	.LCFI21-.Ltext0
	.quad	.LCFI22-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 16
	.quad	.LCFI22-.Ltext0
	.quad	.LCFI23-.Ltext0
	.value	0x2
	.byte	0x76
	.sleb128 16
	.quad	.LCFI23-.Ltext0
	.quad	.LFE7-.Ltext0
	.value	0x2
	.byte	0x77
	.sleb128 8
	.quad	0
	.quad	0
	.section	.debug_aranges,"",@progbits
	.long	0x2c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0
	.value	0
	.value	0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	0
	.quad	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF92:
	.string	"StrIO_ReadString"
.LASF115:
	.string	"GNU Modula-2 4.7.3"
.LASF104:
	.string	".L251"
.LASF5:
	.string	"LONGCARD"
.LASF33:
	.string	"_m2_contents"
.LASF78:
	.string	".L135"
.LASF29:
	.string	"COMPLEX32"
.LASF99:
	.string	"_T86"
.LASF75:
	.string	".L178"
.LASF91:
	.string	".L35"
.LASF80:
	.string	".L206"
.LASF73:
	.string	"high"
.LASF72:
	.string	"_T25"
.LASF71:
	.string	"_T26"
.LASF70:
	.string	"_T27"
.LASF26:
	.string	"REAL64"
.LASF68:
	.string	"_T29"
.LASF89:
	.string	".L54"
.LASF76:
	.string	".L158"
.LASF4:
	.string	"LONGINT"
.LASF3:
	.string	"BYTE"
.LASF117:
	.string	"/home/gaius/GM2/graft-4.7.3/gcc-4.7.3/gcc/gm2/examples/swig/strio"
.LASF18:
	.string	"CARDINAL8"
.LASF47:
	.string	"_T66"
.LASF46:
	.string	"_T67"
.LASF45:
	.string	"_T68"
.LASF116:
	.string	"../../../gm2-libs/StrIO.mod"
.LASF21:
	.string	"CARDINAL64"
.LASF114:
	.string	".L281"
.LASF15:
	.string	"INTEGER16"
.LASF13:
	.string	"SHORTCOMPLEX"
.LASF106:
	.string	".L273"
.LASF19:
	.string	"CARDINAL16"
.LASF85:
	.string	".L56"
.LASF0:
	.string	"CARDINAL"
.LASF107:
	.string	"Echo"
.LASF23:
	.string	"BITSET16"
.LASF25:
	.string	"REAL32"
.LASF34:
	.string	"_m2_high_1"
.LASF87:
	.string	".L211"
.LASF67:
	.string	"_T32"
.LASF119:
	.string	"Erase"
.LASF74:
	.string	".L215"
.LASF2:
	.string	"CHAR"
.LASF66:
	.string	"_T33"
.LASF65:
	.string	"_T34"
.LASF64:
	.string	"_T35"
.LASF63:
	.string	"_T36"
.LASF62:
	.string	"_T37"
.LASF61:
	.string	"_T38"
.LASF100:
	.string	"_T85"
.LASF28:
	.string	"REAL128"
.LASF113:
	.string	".L289"
.LASF51:
	.string	"_T52"
.LASF44:
	.string	"_T71"
.LASF43:
	.string	"_T72"
.LASF42:
	.string	"_T73"
.LASF41:
	.string	"_T74"
.LASF1:
	.string	"INTEGER"
.LASF40:
	.string	"_T77"
.LASF24:
	.string	"BITSET32"
.LASF38:
	.string	"_T79"
.LASF84:
	.string	".L116"
.LASF110:
	.string	".L292"
.LASF8:
	.string	"SHORTREAL"
.LASF11:
	.string	"COMPLEX"
.LASF39:
	.string	"_T78"
.LASF97:
	.string	"_T90"
.LASF17:
	.string	"INTEGER64"
.LASF96:
	.string	"_T91"
.LASF108:
	.string	"AlphaNum"
.LASF9:
	.string	"REAL"
.LASF93:
	.string	"StrIO_WriteString"
.LASF7:
	.string	"SHORTCARD"
.LASF90:
	.string	".L43"
.LASF105:
	.string	".L224"
.LASF60:
	.string	"_T41"
.LASF59:
	.string	"_T42"
.LASF58:
	.string	"_T43"
.LASF57:
	.string	"_T44"
.LASF56:
	.string	"_T45"
.LASF55:
	.string	"_T46"
.LASF54:
	.string	"_T49"
.LASF36:
	.string	"_M2_StrIO_finish"
.LASF83:
	.string	".L72"
.LASF31:
	.string	"COMPLEX96"
.LASF103:
	.string	"_T80"
.LASF86:
	.string	".L64"
.LASF101:
	.string	"_T84"
.LASF32:
	.string	"COMPLEX128"
.LASF14:
	.string	"INTEGER8"
.LASF102:
	.string	"_T83"
.LASF98:
	.string	"_T89"
.LASF79:
	.string	".L122"
.LASF12:
	.string	"LONGCOMPLEX"
.LASF16:
	.string	"INTEGER32"
.LASF20:
	.string	"CARDINAL32"
.LASF77:
	.string	".L160"
.LASF81:
	.string	".L88"
.LASF111:
	.string	".L291"
.LASF69:
	.string	"_T28"
.LASF30:
	.string	"COMPLEX64"
.LASF37:
	.string	"StrIO_WriteLn"
.LASF35:
	.string	"_M2_StrIO_init"
.LASF52:
	.string	"_T51"
.LASF118:
	.string	"ADDRESS"
.LASF112:
	.string	".L285"
.LASF50:
	.string	"_T57"
.LASF10:
	.string	"LONGREAL"
.LASF27:
	.string	"REAL96"
.LASF22:
	.string	"BITSET8"
.LASF6:
	.string	"SHORTINT"
.LASF82:
	.string	".L111"
.LASF88:
	.string	".L49"
.LASF49:
	.string	"_T62"
.LASF53:
	.string	"_T50"
.LASF95:
	.string	"_T92"
.LASF94:
	.string	"_T93"
.LASF109:
	.string	"_T94"
.LASF48:
	.string	"_T65"
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.DW.ref.__gxx_personality_v0,"awG",@progbits,DW.ref.__gxx_personality_v0,comdat
	.align 8
	.type	DW.ref.__gxx_personality_v0, @object
	.size	DW.ref.__gxx_personality_v0, 8
DW.ref.__gxx_personality_v0:
	.quad	__gxx_personality_v0
	.ident	"GCC: (GNU) 4.7.3"
	.section	.note.GNU-stack,"",@progbits
