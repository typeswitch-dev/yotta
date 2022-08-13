[KERNEL-DEFINITIONS]

: PREPARE-REX CALL>
    \ Make sure REX prefix exists, insert it if necessary.
    \ Note that R9 points to the current opcode (after
    \ legacy prefixes and REX prefix if it exists) and
    \ R8 points to the REX prefix. R8 is NULL if the
    \ current instruction doesn't have a REX prefix yet.
    \ So what PREPARE-REX tries to do is create a blank
    \ REX prefix if it's missing, and add R8.
    $4D $85 $C0         \ TEST R8, R8
    $75 $16             \ JNZ +22
    $4D $8B $01         \ MOVQ< R8, [R9]
    $4D $89 $41 $01     \ MOVQ> R8, [R9]. $01
    $4D $8B $C1         \ MOVQ< R8, R9
    $41 $C6 $00 $40     \ MOVB_. [R8] 0x40
    $4D $8D $49 $01     \ LEAQ< R9, [R9]. $01
    $48 $8D $7F $01     \ LEAQ< RDI, [RDI]. $01
    ;

: +W CALL> PREPARE-REX $41 $80 $08 $08 ; \ ORB_. [R8] 0x08
: +R CALL> PREPARE-REX $41 $80 $08 $04 ; \ ORB_. [R8] 0x04
: +X CALL> PREPARE-REX $41 $80 $08 $02 ; \ ORB_. [R8] 0x02
: +B CALL> PREPARE-REX $41 $80 $08 $01 ; \ ORB_. [R8] 0x01

: OPB CALL>
    $4D $33 $C0         \ XOR R8, R8
    $49 $89 $F9 ;       \ MOV R9, RDI

: OPW CALL>
    $B0 $66 $AA         \ MOV AL, 0x66; STOSB
    $4D $33 $C0         \ XOR R8, R8
    $49 $89 $F9 ;       \ MOV R9, RDI

: OPL CALL>
    $4D $33 $C0         \ XOR R8, R8
    $49 $89 $F9 ;       \ MOV R9, RDI

: OPQ CALL>
    $49 $89 $F8         \ MOV R8, RDI
    $B0 $48 $AA         \ MOV AL, 0x48; STOSB
    $49 $89 $F9 ;       \ MOV R9, RDI

\ Opcode extensions. These are modrm bytes with the REG field
\ prefilled, and the MOD field set to 3 (which is the default
\ we use).
: /0 ^B0 ^C0 ^AA ; \ MOV AL, 0xC0; STOSB
: /1 ^B0 ^C8 ^AA ; \ MOV AL, 0xC8; STOSB
: /2 ^B0 ^D0 ^AA ; \ MOV AL, 0xD0; STOSB
: /3 ^B0 ^D8 ^AA ; \ MOV AL, 0xD8; STOSB
: /4 ^B0 ^E0 ^AA ; \ MOV AL, 0xE0; STOSB
: /5 ^B0 ^E8 ^AA ; \ MOV AL, 0xE8; STOSB
: /6 ^B0 ^F0 ^AA ; \ MOV AL, 0xF0; STOSB
: /7 ^B0 ^F8 ^AA ; \ MOV AL, 0xF8; STOSB

[ASSEMBLER-DEFINITIONS]

\ Legacy prefixes
: LOCK ^F0 ; : REP ^F3 ; : REPE ^F3 ; : REPNE ^F2 ;

\ Ops that take no arguments.

: NOP OPL ^90 ; : PAUSE ^F3 OPL ^90 ;
: CBW OPW ^98 ; : CWDE OPL ^98 ; : CDQE OPQ ^98 ;
: CWD OPW ^99 ; : CDQ OPL ^99 ; : CQO OPQ ^99 ;
: FWAIT OPB ^9B ;
: PUSHFW OPW ^9C ; : PUSHFQ OPL ^9C ;
: POPFW OPW ^9D ; : POPFQ OPL ^9D ;

:  INSB OPB ^6C ; :  INSW OPW ^A5 ; :  INSD OPL ^A5 ;
: OUTSB OPB ^6E ; : OUTSW OPW ^A5 ; : OUTSD OPL ^A5 ; : MOVSQ OPQ ^A5 ;
: MOVSB OPB ^A4 ; : MOVSW OPW ^A5 ; : MOVSL OPL ^A5 ; : MOVSQ OPQ ^A5 ;
: CMPSB OPB ^A6 ; : CMPSW OPW ^A7 ; : CMPSL OPL ^A7 ; : CMPSQ OPQ ^A7 ;
: STOSB OPB ^AA ; : STOSW OPW ^AB ; : STOSL OPL ^AB ; : STOSQ OPQ ^AB ;
: LODSB OPB ^AC ; : LODSW OPW ^AD ; : LODSL OPL ^AD ; : LODSQ OPQ ^AD ;
: SCASB OPB ^AE ; : SCASW OPW ^AF ; : SCASL OPL ^AF ; : LODSQ OPQ ^AF ;

: RET   OPL ^C3 ;
: LEAVE OPL ^C9 ;
: FRET  OPL ^CB ;
: INT3  OPB ^CC ;
: IRETW OPW ^CF ; : IRETL OPL ^CF ; : IRETQ OPQ ^CF ;

: XLATB   OPB ^D7 ;

:  INB-DX OPB ^EC ; :  INW-DX OPW ^ED ; :  INL-DX OPL ^ED ;
: OUTB-DX OPB ^EE ; : OUTW-DX OPW ^EF ; : OUTL-DX OPL ^EF ;

: INT1    OPB ^F1 ;
: HLT     OPB ^F4 ;
: CMC     OPB ^F5 ; \ Complement carry flag.
: CLC     OPB ^F8 ; \ Clear carry flag
: STC     OPB ^F9 ; \ Set carry flag
: CLI     OPB ^FA ; \ Clear interrupt flag
: STI     OPB ^FB ; \ Set interrput flag
: CLD     OPB ^FC ; \ Clear direction flag
: STD     OPB ^FD ; \ Set direction flag

: SYSCALL OPL ^0F ^05 ;

\ Ops that expect both a register and modrm argument.
\ E.g.  MOVQ< RBX, [RAX]
: ADDB> OPB ^00 ; : ADCB> OPB ^10 ; : ANDB> OPB ^20 ; : XORB> OPB ^30 ;
: ADDW> OPW ^01 ; : ADCW> OPW ^11 ; : ANDW> OPW ^21 ; : XORW> OPW ^31 ;
: ADDL> OPL ^01 ; : ADCL> OPL ^11 ; : ANDL> OPL ^21 ; : XORL> OPL ^31 ;
: ADDQ> OPQ ^01 ; : ADCQ> OPQ ^11 ; : ANDQ> OPQ ^21 ; : XORQ> OPQ ^31 ;

: ADDB< OPB ^02 ; : ADCB< OPB ^12 ; : ANDB< OPB ^22 ; : XORB< OPB ^32 ;
: ADDW< OPW ^03 ; : ADCW< OPW ^13 ; : ANDW< OPW ^23 ; : XORW< OPW ^33 ;
: ADDL< OPL ^03 ; : ADCL< OPL ^13 ; : ANDL< OPL ^23 ; : XORL< OPL ^33 ;
: ADDQ< OPQ ^03 ; : ADCQ< OPQ ^13 ; : ANDQ< OPQ ^23 ; : XORQ< OPQ ^33 ;

: ORB> OPB ^08 ; : SBBB> OPB ^18 ; : SUBB> OPB ^28 ; : CMPB> OPB ^38 ;
: ORW> OPW ^09 ; : SBBW> OPW ^19 ; : SUBW> OPW ^29 ; : CMPW> OPW ^39 ;
: ORL> OPL ^09 ; : SBBL> OPL ^19 ; : SUBL> OPL ^29 ; : CMPL> OPL ^39 ;
: ORQ> OPQ ^09 ; : SBBQ> OPQ ^19 ; : SUBQ> OPQ ^29 ; : CMPQ> OPQ ^39 ;

: ORB< OPB ^0A ; : SBBB< OPB ^1A ; : SUBB< OPB ^2A ; : CMPB< OPB ^3A ;
: ORW< OPW ^0B ; : SBBW< OPW ^1B ; : SUBW< OPW ^2B ; : CMPW< OPW ^3B ;
: ORL< OPL ^0B ; : SBBL< OPL ^1B ; : SUBL< OPL ^2B ; : CMPL< OPL ^3B ;
: ORQ< OPQ ^0B ; : SBBQ< OPQ ^1B ; : SUBQ< OPQ ^2B ; : CMPQ< OPQ ^3B ;

: TESTB> OPB ^84 ; : XCHGB> OPB ^84 ;
: TESTW> OPW ^85 ; : XCHGW> OPW ^85 ;
: TESTL> OPL ^85 ; : XCHGL> OPL ^85 ;
: TESTQ> OPQ ^85 ; : XCHGQ> OPQ ^85 ;

: MOVB> OPB ^88 ; : MOVB< OPB ^8A ;
: MOVW> OPW ^89 ; : MOVW< OPW ^8B ;
: MOVL> OPL ^89 ; : MOVL< OPL ^8B ;
: MOVQ> OPQ ^89 ; : MOVQ< OPQ ^8B ;

: MOVZXWB< OPW ^0F ^B6 ; : MOVSXWB< OPW ^0F ^BE ;
: MOVZXLB< OPL ^0F ^B6 ; : MOVSXLB< OPL ^0F ^BE ;
: MOVZXQB< OPQ ^0F ^B6 ; : MOVSXQB< OPQ ^0F ^BE ;
: MOVZXLW< OPL ^0F ^B7 ; : MOVSXLW< OPL ^0F ^BF ;
: MOVZXQW< OPQ ^0F ^B7 ; : MOVSXQW< OPQ ^0F ^BF ;
( -------------------- ) : MOVSXQL< OPQ ^63     ;

: LEAW< OPW ^8D ;
: LEAL< OPL ^8D ;
: LEAQ< OPQ ^8D ;

: IMULW< OPW ^0F ^AF ;
: IMULL< OPL ^0F ^AF ;
: IMULQ< OPQ ^0F ^AF ;

\ Ops that expect register, modrm, and immediate arguments.
\ (Do not write a comma after the modrm argument!)
\ E.g.  IMULW<. RAX, RCX $0A
: IMULW<. OPW ^6B ; : IMULW<.. OPW ^69 ;
: IMULL<. OPL ^6B ; : IMULL<:  OPL ^69 ;
: IMULQ<. OPQ ^6B ; : IMULQ<:  OPQ ^69 ;

\ Ops that expect an immediate.
: ADD-AL.  OPB ^04 ; : ADC-AL.  OPB ^14 ;
: ADD-AX.. OPW ^05 ; : ADC-AX.. OPW ^15 ;
: ADD-EAX: OPL ^05 ; : ADC-EAX: OPL ^15 ;
: ADD-RAX: OPQ ^05 ; : ADC-RAX: OPQ ^15 ;

: AND-AL.  OPB ^24 ; : XOR-AL.  OPB ^34 ;
: AND-AX.. OPW ^25 ; : XOR-AX.. OPW ^35 ;
: AND-EAX: OPL ^25 ; : XOR-EAX: OPL ^35 ;
: AND-RAX: OPQ ^25 ; : XOR-RAX: OPQ ^35 ;

: OR-AL.  OPB ^0C ; : SBB-AL.  OPB ^1C ;
: OR-AX.. OPW ^0D ; : SBB-AX.. OPW ^1D ;
: OR-EAX: OPL ^0D ; : SBB-EAX: OPL ^1D ;
: OR-RAX: OPQ ^0D ; : SBB-RAX: OPQ ^1D ;

: SUB-AL.  OPB ^2C ; : CMP-AL.  OPB ^3C ;
: SUB-AX.. OPW ^2D ; : CMP-AX.. OPW ^3C ;
: SUB-EAX: OPL ^2D ; : CMP-EAX: OPL ^3D ;
: SUB-RAX: OPQ ^2D ; : CMP-RAX: OPQ ^3D ;

: PUSHW.. OPW ^68 ;
: PUSHQ.  OPL ^6A ; \ Pushes 64-bits even without REX.W prefix. TODO verify
: PUSHQ:  OPL ^68 ; \ Pushes 64-bits even without REX.W prefix. TODO verify

: MOV-AL-@::  OPB ^A0 ; : MOV-AL-!::  OPB ^A2 ;
: MOV-AX-@::  OPW ^A1 ; : MOV-AX-!::  OPW ^A3 ;
: MOV-EAX-@:: OPL ^A1 ; : MOV-EAX-!:: OPL ^A3 ;
: MOV-RAX-@:: OPQ ^A1 ; : MOV-RAX-!:: OPQ ^A3 ;

: TEST-AL.  OPB ^A8 ; : TEST-AX.. OPW ^A9 ;
: TEST-EAX: OPL ^A9 ; : TEST-RAX: OPQ ^A9 ;

: RET..    OPL ^C2 ; \ RET while popping extra bytes
: ENTER... OPL ^C8 ;
: FRET..   OPL ^CA ;
: INT.     OPB ^CD ;

:  INB. OPB ^E4 ; :  INW. OPW ^E5 ; :  INL. OPL ^E5 ;
: OUTB. OPB ^E6 ; : OUTW. OPW ^E7 ; : OUTL. OPL ^E7 ;

\ Short jumps. These expect an immediate signed byte offset.
: JO.  ^70 ; : JNO. ^71 ; : JB.  ^72 ; : JAE. ^73 ;
: JZ.  ^74 ; : JNZ. ^75 ; : JBE. ^76 ; : JA.  ^77 ;
: JS.  ^78 ; : JNS. ^79 ; : JPE. ^7A ; : JPO. ^7B ;
: JL.  ^7C ; : JGE. ^7D ; : JLE. ^7E ; : JG.  ^7F ;
: JMP. ^EB ; : JE.  ^74 ; : JNE. ^75 ;
: LOOPNE. ^E0 ; : LOOPE. ^E1 ; : LOOP. ^E2 ; : JRCXZ. ^E3 ;

\ Near jumps. These expect an immediate signed dword offset.
:  JO: ^0F ^80 ; : JNO: ^0F ^81 ; :  JB: ^0F ^82 ; : JAE: ^0F ^83 ;
:  JZ: ^0F ^84 ; : JNZ: ^0F ^85 ; : JBE: ^0F ^86 ; :  JA: ^0F ^87 ;
:  JS: ^0F ^88 ; : JNS: ^0F ^89 ; : JPE: ^0F ^8A ; : JPO: ^0F ^8B ;
:  JL: ^0F ^8C ; : JGE: ^0F ^8D ; : JLE: ^0F ^8E ; :  JG: ^0F ^8F ;
: JMP: ^E9     ; :  JE: ^0F ^84 ; : JNE: ^0F ^85 ;
: CALL: ^E8 ;

\ Ops that expect a direct register argument.
\ (Do not write a comma after the direct register argument!)
\ E.g. PUSHQ# RAX
\      POPQ# R15
: PUSHW# OPW ^50 ;
: POPW#  OPW ^58 ;
: PUSHQ# OPL ^50 ; \ forced to 64 bits
: POPQ#  OPL ^58 ; \ forced to 64 bits

: XCHGW-AX# OPW ^90 ;
: XCHGL-EAX# OPL ^90 ;
: XCHGQ-RAX# OPQ ^90 ;

\ Ops that expect a direct register argument and an immediate.
\ (Do not write a comma after the direct register argument!)
\ E.g.  MOVL#: RAX $10203040  ( MOV EAX, 10203040h )
\       MOVW#: R13 $1020      ( MOV R13, 1020h )
: MOVB#.  OPB ^B0 ;
: MOVW#.. OPW ^B8 ;
: MOVL#:  OPL ^B8 ;
: MOVQ#:: OPQ ^B8 ;

\ Ops that expect a modrm argument only.

: SHLB_1 OPB ^D0 /4 ; : SHRB_1 OPB ^D0 /5 ; : SARB_1 OPB ^D0 /7 ;
: SHLW_1 OPW ^D1 /4 ; : SHRW_1 OPW ^D1 /5 ; : SARW_1 OPW ^D1 /7 ;
: SHLL_1 OPL ^D1 /4 ; : SHRL_1 OPL ^D1 /5 ; : SARL_1 OPL ^D1 /7 ;
: SHLQ_1 OPQ ^D1 /4 ; : SHRQ_1 OPQ ^D1 /5 ; : SARQ_1 OPQ ^D1 /7 ;

: SHLB_CL OPB ^D2 /4 ; : SHRB_CL OPB ^D2 /5 ; : SARB_CL OPB ^D2 /7 ;
: SHLW_CL OPW ^D3 /4 ; : SHRW_CL OPW ^D3 /5 ; : SARW_CL OPW ^D3 /7 ;
: SHLL_CL OPL ^D3 /4 ; : SHRL_CL OPL ^D3 /5 ; : SARL_CL OPL ^D3 /7 ;
: SHLQ_CL OPQ ^D3 /4 ; : SHRQ_CL OPQ ^D3 /5 ; : SARQ_CL OPQ ^D3 /7 ;

: SETO_  OPB ^0F ^90 /0 ; : SETNO_ OPB ^0F ^91 /0 ;
: SETB_  OPB ^0F ^92 /0 ; : SETAE_ OPB ^0F ^93 /0 ;
: SETZ_  OPB ^0F ^94 /0 ; : SETNZ_ OPB ^0F ^95 /0 ;
: SETBE_ OPB ^0F ^96 /0 ; : SETA_  OPB ^0F ^97 /0 ;
: SETS_  OPB ^0F ^98 /0 ; : SETNS_ OPB ^0F ^99 /0 ;
: SETPE_ OPB ^0F ^9A /0 ; : SETPO_ OPB ^0F ^9B /0 ;
: SETL_  OPB ^0F ^9C /0 ; : SETGE_ OPB ^0F ^9D /0 ;
: SETLE_ OPB ^0F ^9E /0 ; : SETG_  OPB ^0F ^9F /0 ;

:  POPW_ OPW ^8F /0 ;
: PUSHW_ OPW ^FF /6 ;
:  POPQ_ OPL ^8F /0 ; \ forced to 64 bits
: PUSHQ_ OPL ^FF /6 ; \ forced to 64 bits

: NOTB_ OPB ^F6 /2 ; : NEGB_  OPB ^F6 /3 ;
: NOTW_ OPW ^F7 /2 ; : NEGW_  OPW ^F7 /3 ;
: NOTL_ OPL ^F7 /2 ; : NEGL_  OPL ^F7 /3 ;
: NOTQ_ OPQ ^F7 /2 ; : NEGQ_  OPQ ^F7 /3 ;
: MULB_ OPB ^F6 /4 ; : IMULB_ OPB ^F6 /5 ;
: MULW_ OPW ^F7 /4 ; : IMULW_ OPW ^F7 /5 ;
: MULL_ OPL ^F7 /4 ; : IMULL_ OPL ^F7 /5 ;
: MULQ_ OPQ ^F7 /4 ; : IMULQ_ OPQ ^F7 /5 ;
: DIVB_ OPB ^F6 /6 ; : IDIVB_ OPB ^F6 /7 ;
: DIVW_ OPW ^F7 /6 ; : IDIVW_ OPW ^F7 /7 ;
: DIVL_ OPL ^F7 /6 ; : IDIVL_ OPL ^F7 /7 ;
: DIVQ_ OPQ ^F7 /6 ; : IDIVQ_ OPQ ^F7 /7 ;

: INCB_ OPB ^FE /0 ; : DECB_ OPB ^FE /1 ;
: INCW_ OPW ^FF /0 ; : DECW_ OPW ^FF /1 ;
: INCL_ OPL ^FF /0 ; : DECL_ OPL ^FF /1 ;
: INCQ_ OPQ ^FF /0 ; : DECQ_ OPQ ^FF /1 ;

: CALLQ_ OPL ^FF /2 ; \ forced to 64 bits
: JUMPQ_ OPL ^FF /4 ; \ forced to 64 bits

\ Ops that expect a modrm argument and an immediate.

: ADDB_.  OPB ^80 /0 ; :  ORB_.  OPB ^80 /1 ;
: ADDW_.  OPW ^83 /0 ; :  ORW_.  OPW ^83 /1 ;
: ADDL_.  OPL ^83 /0 ; :  ORL_.  OPL ^83 /1 ;
: ADDQ_.  OPQ ^83 /0 ; :  ORQ_.  OPQ ^83 /1 ;
: ADDW_.. OPW ^81 /0 ; :  ORW_.. OPW ^81 /1 ;
: ADDL_:  OPL ^81 /0 ; :  ORL_:  OPL ^81 /1 ;
: ADDQ_:  OPQ ^81 /0 ; :  ORQ_:  OPQ ^81 /1 ;

: ADCB_.  OPB ^80 /2 ; : SBBB_.  OPB ^80 /3 ;
: ADCW_.  OPW ^83 /2 ; : SBBW_.  OPW ^83 /3 ;
: ADCL_.  OPL ^83 /2 ; : SBBL_.  OPL ^83 /3 ;
: ADCQ_.  OPQ ^83 /2 ; : SBBQ_.  OPQ ^83 /3 ;
: ADCW_.. OPW ^81 /2 ; : SBBW_.. OPW ^81 /3 ;
: ADCL_:  OPL ^81 /2 ; : SBBL_:  OPL ^81 /3 ;
: ADCQ_:  OPQ ^81 /2 ; : SBBQ_:  OPQ ^81 /3 ;

: ANDB_.  OPB ^80 /4 ; : SUBB_.  OPB ^80 /5 ;
: ANDW_.  OPW ^83 /4 ; : SUBW_.  OPW ^83 /5 ;
: ANDL_.  OPL ^83 /4 ; : SUBL_.  OPL ^83 /5 ;
: ANDQ_.  OPQ ^83 /4 ; : SUBQ_.  OPQ ^83 /5 ;
: ANDW_.. OPW ^81 /4 ; : SUBW_.. OPW ^81 /5 ;
: ANDL_:  OPL ^81 /4 ; : SUBL_:  OPL ^81 /5 ;
: ANDQ_:  OPQ ^81 /4 ; : SUBQ_:  OPQ ^81 /5 ;

: XORB_.  OPB ^80 /6 ; : CMPB_.  OPB ^80 /7 ;
: XORW_.  OPW ^83 /6 ; : CMPW_.  OPW ^83 /7 ;
: XORL_.  OPL ^83 /6 ; : CMPL_.  OPL ^83 /7 ;
: XORQ_.  OPQ ^83 /6 ; : CMPQ_.  OPQ ^83 /7 ;
: XORW_.. OPW ^81 /6 ; : CMPW_.. OPW ^81 /7 ;
: XORL_:  OPL ^81 /6 ; : CMPL_:  OPL ^81 /7 ;
: XORQ_:  OPQ ^81 /6 ; : CMPQ_:  OPQ ^81 /7 ;

: SHLB_. OPB ^C0 /4 ; : SHRB_. OPB ^C0 /5 ; : SARB_. OPB ^C0 /7 ;
: SHLW_. OPW ^C1 /4 ; : SHRW_. OPW ^C1 /5 ; : SARW_. OPW ^C1 /7 ;
: SHLL_. OPL ^C1 /4 ; : SHRL_. OPL ^C1 /5 ; : SARL_. OPL ^C1 /7 ;
: SHLQ_. OPQ ^C1 /4 ; : SHRQ_. OPQ ^C1 /5 ; : SARQ_. OPQ ^C1 /7 ;

: MOVB_.  OPB ^C6 /0 ;
: MOVW_.. OPW ^C7 /0 ;
: MOVL_:  OPL ^C7 /0 ;
: MOVQ_:  OPQ ^C7 /0 ; \ 32-bit immediate sign-extended to 64-bits

: TESTB_.  OPB ^F6 /0 ;
: TESTW_.. OPW ^F7 /0 ;
: TESTL_:  OPL ^F7 /0 ;
: TESTQ_:  OPQ ^F7 /0 ; \ 32-bit immediate sign-extended to 64-bits

\ Encoding the ModR/M byte starting with the REG field.
\ We set MOD=3 by default, so we can reuse plain register names
\ for some operands (see above).
: RAX, ^C0    ; : RCX, ^C8    ; : RDX, ^D0    ; : RBX, ^D8    ;
: RSP, ^E0    ; : RBP, ^E8    ; : RSI, ^F0    ; : RDI, ^F8    ;
: R8,  ^C0 +R ; : R9,  ^C8 +R ; : R10, ^D0 +R ; : R11, ^D8 +R ;
: R12, ^E0 +R ; : R13, ^E8 +R ; : R14, ^F0 +R ; : R15, ^F8 +R ;

[KERNEL-DEFINITIONS]

\ Modify ModR/M byte to set MOD field.
: MOD=0 ^80 ^77 ^FF ^C0 ; \ XOR [RDI-1], 0xC0
: MOD=1 ^80 ^77 ^FF ^80 ; \ XOR [RDI-1], 0x80
: MOD=2 ^80 ^77 ^FF ^40 ; \ XOR [RDI-1], 0x40
: MOD=3                 ; \ default MOD is 3

\ Modify ModR/M byte to set RM field.
\ (This is also used by SIB encoding to modify the BASE field.)
: RM=0                 ; \ default RM is 0
: RM=1 ^80 ^77 ^FF ^01 ; \ XOR [RDI-1], 0x01
: RM=2 ^80 ^77 ^FF ^02 ; \ XOR [RDI-1], 0x02
: RM=3 ^80 ^77 ^FF ^03 ; \ XOR [RDI-1], 0x03
: RM=4 ^80 ^77 ^FF ^04 ; \ XOR [RDI-1], 0x04
: RM=5 ^80 ^77 ^FF ^05 ; \ XOR [RDI-1], 0x05
: RM=6 ^80 ^77 ^FF ^06 ; \ XOR [RDI-1], 0x06
: RM=7 ^80 ^77 ^FF ^07 ; \ XOR [RDI-1], 0x07

[ASSEMBLER-DEFINITIONS]

\ Direct register addressing.
: RAX MOD=3 RM=0 ; : R8  MOD=3 RM=0 +B ;
: RCX MOD=3 RM=1 ; : R9  MOD=3 RM=1 +B ;
: RDX MOD=3 RM=2 ; : R10 MOD=3 RM=2 +B ;
: RBX MOD=3 RM=3 ; : R11 MOD=3 RM=3 +B ;
: RSP MOD=3 RM=4 ; : R12 MOD=3 RM=4 +B ;
: RBP MOD=3 RM=5 ; : R13 MOD=3 RM=5 +B ;
: RSI MOD=3 RM=6 ; : R14 MOD=3 RM=6 +B ;
: RDI MOD=3 RM=7 ; : R15 MOD=3 RM=7 +B ;

\ Indirect addressing, with or without offsets.
: [RAX]  MOD=0 RM=0    ; : [RAX]. MOD=1 RM=0    ; : [RAX]: MOD=2 RM=0    ;
: [RCX]  MOD=0 RM=1    ; : [RCX]. MOD=1 RM=1    ; : [RCX]: MOD=2 RM=1    ;
: [RDX]  MOD=0 RM=2    ; : [RDX]. MOD=1 RM=2    ; : [RDX]: MOD=2 RM=2    ;
: [RBX]  MOD=0 RM=3    ; : [RBX]. MOD=1 RM=3    ; : [RBX]: MOD=2 RM=3    ;
( --------------------------------- SIB -------------------------------- )
: [RIP]: MOD=0 RM=5    ; : [RBP]. MOD=1 RM=5    ; : [RBP]: MOD=2 RM=5    ;
: [RSI]  MOD=0 RM=6    ; : [RSI]. MOD=1 RM=6    ; : [RSI]: MOD=2 RM=6    ;
: [RDI]  MOD=0 RM=7    ; : [RDI]. MOD=1 RM=7    ; : [RDI]: MOD=2 RM=7    ;
: [R8]   MOD=0 RM=0 +B ; : [R8].  MOD=1 RM=0 +B ; : [R8]:  MOD=2 RM=0 +B ;
: [R9]   MOD=0 RM=1 +B ; : [R9].  MOD=1 RM=1 +B ; : [R9]:  MOD=2 RM=1 +B ;
: [R10]  MOD=0 RM=2 +B ; : [R10]. MOD=1 RM=2 +B ; : [R10]: MOD=2 RM=2 +B ;
: [R11]  MOD=0 RM=3 +B ; : [R11]. MOD=1 RM=3 +B ; : [R11]: MOD=2 RM=3 +B ;
( --------------------------------- SIB -------------------------------- )
( -------- RIP ------- ) : [R13]. MOD=1 RM=5 +B ; : [R13]: MOD=2 RM=5 +B ;
: [R14]  MOD=0 RM=6 +B ; : [R14]. MOD=1 RM=6 +B ; : [R14]: MOD=2 RM=6 +B ;
: [R15]  MOD=0 RM=7 +B ; : [R15]. MOD=1 RM=7 +B ; : [R15]: MOD=2 RM=7 +B ;

\ Start encoding a SIB byte, starting with the SCALE and INDEX fields.
: [1*RAX+ ^00    ; : [2*RAX+ ^40    ; : [4*RAX+ ^80    ; : [8*RAX+ ^C0    ;
: [1*RCX+ ^08    ; : [2*RCX+ ^48    ; : [4*RCX+ ^88    ; : [8*RCX+ ^C8    ;
: [1*RDX+ ^10    ; : [2*RDX+ ^50    ; : [4*RDX+ ^90    ; : [8*RDX+ ^D0    ;
: [1*RBX+ ^18    ; : [2*RBX+ ^58    ; : [4*RBX+ ^98    ; : [8*RBX+ ^D8    ;
: [0+     ^20    ; : [2*0+   ^60    ; : [4*0+   ^A0    ; : [8*0+   ^E0    ;
: [1*RBP+ ^28    ; : [2*RBP+ ^68    ; : [4*RBP+ ^A8    ; : [8*RBP+ ^E8    ;
: [1*RSI+ ^30    ; : [2*RSI+ ^70    ; : [4*RSI+ ^B0    ; : [8*RSI+ ^F0    ;
: [1*RDI+ ^38    ; : [2*RDI+ ^78    ; : [4*RDI+ ^B8    ; : [8*RDI+ ^F8    ;
: [1*R8+  ^00 +X ; : [2*R8+  ^40 +X ; : [4*R8+  ^80 +X ; : [8*R8+  ^C0 +X ;
: [1*R9+  ^08 +X ; : [2*R9+  ^48 +X ; : [4*R9+  ^88 +X ; : [8*R9+  ^C8 +X ;
: [1*R10+ ^10 +X ; : [2*R10+ ^50 +X ; : [4*R10+ ^90 +X ; : [8*R10+ ^D0 +X ;
: [1*R11+ ^18 +X ; : [2*R10+ ^58 +X ; : [4*R11+ ^98 +X ; : [8*R11+ ^D8 +X ;
: [1*R12+ ^20 +X ; : [2*R12+ ^60 +X ; : [4*R12+ ^A0 +X ; : [8*R12+ ^E0 +X ;
: [1*R13+ ^28 +X ; : [2*R13+ ^68 +X ; : [4*R13+ ^A8 +X ; : [8*R13+ ^E8 +X ;
: [1*R14+ ^30 +X ; : [2*R14+ ^70 +X ; : [4*R14+ ^B0 +X ; : [8*R14+ ^F0 +X ;
: [1*R15+ ^38 +X ; : [2*R15+ ^78 +X ; : [4*R15+ ^B8 +X ; : [8*R15+ ^F8 +X ;

\ Modify the previous byte (ModR/M) to encode the appropriate SIB mode.
: SIB=0 ^80 ^77 ^FE ^C4 ; \ set prev MOD=0 RM=4
: SIB=1 ^80 ^77 ^FE ^84 ; \ set prev MOD=1 RM=4
: SIB=2 ^80 ^77 ^FE ^44 ; \ set prev MOD=2 RM=4

\ Finish encoding SIB byte. This sets the BASE field and MOD from ModR/M.
: RAX] SIB=0 RM=0    ; : RAX]. SIB=1 RM=0    ; : RAX]: SIB=2 RM=0    ;
: RCX] SIB=0 RM=1    ; : RCX]. SIB=1 RM=1    ; : RCX]: SIB=2 RM=1    ;
: RDX] SIB=0 RM=2    ; : RDX]. SIB=1 RM=2    ; : RDX]: SIB=2 RM=2    ;
: RBX] SIB=0 RM=3    ; : RBX]. SIB=1 RM=3    ; : RBX]: SIB=2 RM=3    ;
: RSP] SIB=0 RM=4    ; : RSP]. SIB=1 RM=4    ; : RSP]: SIB=2 RM=4    ;
: 0]:  SIB=0 RM=5    ; : RBP]. SIB=1 RM=5    ; : RBP]: SIB=2 RM=5    ;
: RSI] SIB=0 RM=6    ; : RSI]. SIB=1 RM=6    ; : RSI]: SIB=2 RM=6    ;
: RDI] SIB=0 RM=7    ; : RDI]. SIB=1 RM=7    ; : RDI]: SIB=2 RM=7    ;
: R8]  SIB=0 RM=0 +B ; : R8].  SIB=1 RM=0 +B ; : R8]:  SIB=2 RM=0 +B ;
: R9]  SIB=0 RM=1 +B ; : R9].  SIB=1 RM=1 +B ; : R9]:  SIB=2 RM=1 +B ;
: R10] SIB=0 RM=2 +B ; : R10]. SIB=1 RM=2 +B ; : R10]: SIB=2 RM=2 +B ;
: R11] SIB=0 RM=3 +B ; : R11]. SIB=1 RM=3 +B ; : R11]: SIB=2 RM=3 +B ;
: R12] SIB=0 RM=4 +B ; : R12]. SIB=1 RM=4 +B ; : R12]: SIB=2 RM=4 +B ;
( ------------------ ) : R13]. SIB=1 RM=5 +B ; : R13]: SIB=2 RM=5 +B ;
: R14] SIB=0 RM=6 +B ; : R14]. SIB=1 RM=6 +B ; : R14]: SIB=2 RM=6 +B ;
: R15] SIB=0 RM=7 +B ; : R15]. SIB=1 RM=7 +B ; : R15]: SIB=2 RM=7 +B ;

