	.file	"sets.mod"
	.stabs	"/home/gaius/GM2/gcc-3.3.3/gcc/testsuite/gm2/pim/pass/",100,0,0,.Ltext0
	.stabs	"sets.mod",100,0,0,.Ltext0
	.text
.Ltext0:
	.stabs	"gcc2_compiled.",60,0,0,0
	.stabs	"int:t(0,1)=r(0,1);-2147483648;2147483647;",128,0,0,0
	.stabs	"char:t(0,2)=r(0,2);0;127;",128,0,0,0
	.stabs	"long int:t(0,3)=@s64;r(0,3);001000000000000000000000;000777777777777777777777;",128,0,0,0
	.stabs	"unsigned int:t(0,4)=r(0,4);000000000000000000000000;000000000000037777777777;",128,0,0,0
	.stabs	"long unsigned int:t(0,5)=@s64;r(0,5);000000000000000000000000;001777777777777777777777;",128,0,0,0
	.stabs	"long long int:t(0,6)=@s64;r(0,6);001000000000000000000000;000777777777777777777777;",128,0,0,0
	.stabs	"long long unsigned int:t(0,7)=@s64;r(0,7);000000000000000000000000;001777777777777777777777;",128,0,0,0
	.stabs	"short int:t(0,8)=@s16;r(0,8);-32768;32767;",128,0,0,0
	.stabs	"short unsigned int:t(0,9)=@s16;r(0,9);0;65535;",128,0,0,0
	.stabs	"signed char:t(0,10)=@s8;r(0,10);-128;127;",128,0,0,0
	.stabs	"unsigned char:t(0,11)=@s8;r(0,11);0;255;",128,0,0,0
	.stabs	"INTEGER:t(0,12)=(0,13)=r(0,13);-2147483648;2147483647;",128,0,1,0
	.stabs	"CHAR:t(0,14)=(0,15)=@s8;-20;",128,0,1,0
	.stabs	"CARDINAL:t(0,16)=(0,17)=r(0,17);000000000000000000000000;000000000000037777777777;",128,0,1,0
	.stabs	"BYTE:t(0,18)=(0,11)",128,0,1,0
	.stabs	"WORD:t(0,19)=(0,4)",128,0,1,0
	.stabs	"PROC:t(0,20)=(0,21)=*(0,22)=f(0,23)=(0,23)",128,0,1,0
	.stabs	"ADDRESS:t(0,24)=(0,25)=*(0,23)",128,0,1,0
	.stabs	"LONGINT:t(0,26)=(0,27)=@s64;r(0,27);001000000000000000000000;000777777777777777777777;",128,0,1,0
	.stabs	"LONGCARD:t(0,28)=(0,29)=@s64;r(0,29);000000000000000000000000;001777777777777777777777;",128,0,1,0
	.stabs	"SHORTREAL:t(0,30)=(0,31)=r(0,1);4;0;",128,0,1,0
	.stabs	"REAL:t(0,32)=(0,33)=r(0,1);8;0;",128,0,1,0
	.stabs	"LONGREAL:t(0,34)=(0,35)=r(0,1);16;0;",128,0,1,0
	.stabs	"BITNUM:t(0,36)=(0,37)=r(0,4);0;31;",128,0,1,0
	.stabs	"BITSET:t(0,38)=(0,39)=@s32;S(0,37)",128,0,1,0
	.stabs	"BOOLEAN:T(0,40)=eFALSE:0,TRUE:1,;",128,0,0,0
	.stabs	"colours:T(0,41)=ered:0,blue:1,yellow:2,orange:3,green:4,;",128,0,0,0
	.stabs	"_Unbounded:T(0,42)=s16address:(0,24),0,64;HIGH:(0,16),64,32;;",128,0,0,0
	.stabs	"_M2_sets_init:F(0,1)",36,0,29,_M2_sets_init
.globl _M2_sets_init
	.type	_M2_sets_init, @function
_M2_sets_init:
	.stabn 68,0,29,.LM1-_M2_sets_init
.LM1:
.LFB3:
	pushq	%rbp
.LCFI0:
	movq	%rsp, %rbp
.LCFI1:
	.stabn 68,0,30,.LM2-_M2_sets_init
.LM2:
	movl	$18, %edi
	call	testing
	.stabn 68,0,31,.LM3-_M2_sets_init
.LM3:
	leave
	ret
.LFE3:
	.size	_M2_sets_init, .-_M2_sets_init
.Lscope0:
	.stabs	"",36,0,0,.Lscope0-_M2_sets_init
	.stabs	"testing:f(0,23)",36,0,1,testing
	.stabs	"first:p(0,4)",160,0,1,-4
	.type	testing, @function
testing:
	.stabn 68,0,25,.LM4-testing
.LM4:
.LFB4:
	pushq	%rbp
.LCFI2:
	movq	%rsp, %rbp
.LCFI3:
	movl	%edi, -4(%rbp)
	.stabn 68,0,26,.LM5-testing
.LM5:
	leave
	ret
.LFE4:
	.size	testing, .-testing
.Lscope1:
	.stabs	"",36,0,0,.Lscope1-testing
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	""
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.quad	.LFB3
	.quad	.LFE3-.LFB3
	.byte	0x4
	.long	.LCFI0-.LFB3
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.quad	.LFB4
	.quad	.LFE4-.LFB4
	.byte	0x4
	.long	.LCFI2-.LFB4
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI3-.LCFI2
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE3:
	.text
	.stabs "",100,0,0,.Letext
.Letext:
	.section	.note.GNU-stack,"",@progbits
	.ident	"GCC: (GNU) 3.3.3"
