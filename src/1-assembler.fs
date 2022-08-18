[KERNEL-DEFINITIONS]

: NO-REX/VEX ( -- R8:0 ) CALL>
    $4D $33 $C0 ;       \ XOR R8, R8
: REX/VEX ( -- R8:RDI ) CALL>
    $49 $89 $F8 ;       \ MOV R8, RDI
: OPCODE ( -- R9:RDI ) CALL>
    $49 $89 $F9 ;       \ MOV R9, RDI

: SHIFT-OP ( R9 RDI -- R9+1 RDI+1 ) CALL>
    \ Shift the entire OP up by one byte to make room for prefix.
    $50                 \ PUSH RAX
    $49 $8B $41 $08     \ MOVQ< RAX, [R9]. $08
    $49 $89 $41 $09     \ MOVQ> RAX, [R9]. $09
    $49 $8B $41 $00     \ MOVQ< RAX, [R9]. $00
    $49 $89 $41 $01     \ MOVQ> RAX, [R9]. $01
    $4D $8D $49 $01     \ LEAQ< R9, [R9]. $01
    $48 $8D $7F $01     \ LEAQ< RDI, [RDI]. $01
    $58                 \ POP RAX
    ;

: IS-REX ( R8 -- R8 ZF:is-rex ) CALL>
    \ Check that R8 points to a REX prefix byte.
    \ Result is returned in ZF:
    \   ZF=1 means it is a REX prefix.
    \   ZF=0 means it's not a REX prefix.
    $50                 \ PUSHQ# RAX
    $41 $8A $00         \ MOVB< AL, [R8]
    $24 $F0             \ AND-AL. $F0
    $3C $40             \ CMP-AL. $40
    $58                 \ POPQ# RAX
    ;

: IS-VEX2 ( R8 -- R8 ZF:is-vex2 ) CALL>
    \ Check that R8 points to the start of a 2-byte VEX prefix.
    \ Result is returned in ZF:
    \   ZF=1 means it is 2-byte VEX prefix.
    \   ZF=0 means it's not a 2-byte VEX prefix.
    $41 $80 $38 $C5     \ CMPB_. [R8] $C5
    ;

: IS-VEX3 ( R8 -- R8 ZF:is-vex3 ) CALL>
    \ Check that R8 points to the start of a 3-byte VEX prefix.
    \ Result is returned in ZF:
    \   ZF=1 means it is 3-byte VEX prefix.
    \   ZF=0 means it's not a 3-byte VEX prefix.
    $41 $80 $38 $C4     \ CMPB_. [R8] $C4
    ;

 : IS-EVEX ( R8 -- R8 ZF:is-evex ) CALL>
    \ Check that R8 points to the start of a EVEX prefix.
    \ Result is returned in ZF:
    \   ZF=1 means it is EVEX prefix.
    \   ZF=0 means it's not a EVEX prefix.
    $41 $80 $38 $62     \ CMPB_. [R8] $62
    ;

: PREPARE-REX ( R8:x|0 R9:op -- R8:x|REX R9:op ZF:is-rex ) CALL>
    \ Make sure REX prefix exists, insert it if necessary.
    \ Note that R9 points to the current opcode (after
    \ legacy prefixes and REX prefix if it exists) and
    \ R8 points to the REX prefix. R8 is NULL if the
    \ current instruction doesn't have a REX prefix yet.
    \ So what PREPARE-REX tries to do is create a blank
    \ REX prefix if it's missing.
    \ If a VEX or EVEX prefix exists instead, this word
    \ leaves them alone, does not insert a REX prefix.
    $4D $85 $C0         \ TEST R8, R8
    $75 $0C             \ JNZ +12
    $4D $8B $C1         \ MOVQ< R8, R9
    SHIFT-OP
    $41 $C6 $00 $40     \ MOVB_. [R8] 0x40
    IS-REX
    ;

: PREPARE-VEX3 ( R8:x|VEX2 R9:op -- R8:x|VEX3 R9:op ZF:is-vex3 ) CALL>
    \ Convert a 2-byte VEX prefix, if present, into
    \ a 3-byte VEX prefix. The 3-byte version is needed
    \ to encode W,X,B bits, and a different map_select.
    IS-VEX2
    $75 $1F             \ JNZ +31
    SHIFT-OP
    $41 $C6 $00 $40     \ MOVB_. [R8] 0xC4
    $41 $8A $40 $01     \ MOVB< RAX, [R8]. 0x01
    $24 $7F             \ AND-AL. 0x7F
    $41 $88 $40 $02     \ MOVB> RAX, [R8]. 0x02
    $41 $8A $40 $01     \ MOVB< RAX, [R8]. 0x01
    $24 $80             \ AND-AL. 0x80
    $0C $61             \ OR-AL. 0x61
    $41 $88 $40 $01     \ MOVB> RAX, [R8]. 0x01
    IS-VEX3
    ;

: +W CALL>
    PREPARE-REX
    $75 $05             \ JNZ +05
    $41 $80 $08 $08     \ ORB_. [R8] 0x08
    $C3                 \ RET
    PREPARE-VEX3
    $75 $06             \ JNZ +06
    $41 $80 $48 $02 $08 \ ORB_. [R8]. 0x02 0x08
    $C3                 \ RET
    ;                   \ TODO handle EVEX and/or panic

: +R CALL>
    PREPARE-REX
    $75 $05             \ JNZ +5
    $41 $80 $08 $04     \ ORB_. [R8] 0x04
    $C3                 \ RET
    IS-VEX2
    $74 $07             \ JZ +7
    IS-VEX3
    $75 $04             \ JNZ +6
    $41 $80 $60 $01 $7F \ ANDB_. [R8]. 0x01 0x7F
    $C3                 \ RET
    ;                   \ TODO handle EVEX and/or panic

: +X CALL>
    PREPARE-REX
    $75 $05             \ JNZ +05
    $41 $80 $08 $02     \ ORB_. [R8] 0x02
    $C3                 \ RET
    PREPARE-VEX3
    $75 $06             \ JNZ +06
    $41 $80 $60 $01 $BF \ ANDB_. [R8]. 0x01 0xBF
    $C3                 \ RET
    ;                   \ TODO handle EVEX and/or panic

: +B CALL>
    PREPARE-REX
    $75 $05             \ JNZ +05
    $41 $80 $08 $01     \ ORB_. [R8] 0x01
    $C3                 \ RET
    PREPARE-VEX3
    $75 $06             \ JNZ +06
    $41 $80 $60 $01 $DF \ ANDB_. [R8]. 0x01 0xDF
    $C3                 \ RET
    ;                   \ TODO handle EVEX and/or panic

: OPB CALL>     NO-REX/VEX     OPCODE ;
: OPW CALL> ^66 NO-REX/VEX     OPCODE ;
: OPL CALL>     NO-REX/VEX     OPCODE ;
: OPQ CALL>        REX/VEX ^48 OPCODE ;

:      VEX.128.0F    CALL> REX/VEX ^C5     ^F8 OPCODE ;
:   VEX.128.66.0F    CALL> REX/VEX ^C5     ^F9 OPCODE ;
:   VEX.128.F3.0F    CALL> REX/VEX ^C5     ^FA OPCODE ;
:   VEX.128.F2.0F    CALL> REX/VEX ^C5     ^FB OPCODE ;
:      VEX.256.0F    CALL> REX/VEX ^C5     ^FC OPCODE ;
:   VEX.256.66.0F    CALL> REX/VEX ^C5     ^FD OPCODE ;
:   VEX.256.F3.0F    CALL> REX/VEX ^C5     ^FE OPCODE ;
:   VEX.256.F2.0F    CALL> REX/VEX ^C5     ^FF OPCODE ;
:    VEX.128.0F38    CALL> REX/VEX ^C4 ^E2 ^78 OPCODE ;
: VEX.128.66.0F38    CALL> REX/VEX ^C4 ^E2 ^79 OPCODE ;
: VEX.128.F3.0F38    CALL> REX/VEX ^C4 ^E2 ^7A OPCODE ;
: VEX.128.F2.0F38    CALL> REX/VEX ^C4 ^E2 ^7B OPCODE ;
:    VEX.256.0F38    CALL> REX/VEX ^C4 ^E2 ^7C OPCODE ;
: VEX.256.66.0F38    CALL> REX/VEX ^C4 ^E2 ^7D OPCODE ;
: VEX.256.F3.0F38    CALL> REX/VEX ^C4 ^E2 ^7E OPCODE ;
: VEX.256.F2.0F38    CALL> REX/VEX ^C4 ^E2 ^7F OPCODE ;
:    VEX.128.0F3A    CALL> REX/VEX ^C4 ^E3 ^78 OPCODE ;
: VEX.128.66.0F3A    CALL> REX/VEX ^C4 ^E3 ^79 OPCODE ;
: VEX.128.F3.0F3A    CALL> REX/VEX ^C4 ^E3 ^7A OPCODE ;
: VEX.128.F2.0F3A    CALL> REX/VEX ^C4 ^E3 ^7B OPCODE ;
:    VEX.256.0F3A    CALL> REX/VEX ^C4 ^E3 ^7C OPCODE ;
: VEX.256.66.0F3A    CALL> REX/VEX ^C4 ^E3 ^7D OPCODE ;
: VEX.256.F3.0F3A    CALL> REX/VEX ^C4 ^E3 ^7E OPCODE ;
: VEX.256.F2.0F3A    CALL> REX/VEX ^C4 ^E3 ^7F OPCODE ;
:      VEX.128.0F.W1 CALL> REX/VEX ^C4 ^E1 ^F8 OPCODE ;
:   VEX.128.66.0F.W1 CALL> REX/VEX ^C4 ^E1 ^F9 OPCODE ;
:   VEX.128.F3.0F.W1 CALL> REX/VEX ^C4 ^E1 ^FA OPCODE ;
:   VEX.128.F2.0F.W1 CALL> REX/VEX ^C4 ^E1 ^FB OPCODE ;
:      VEX.256.0F.W1 CALL> REX/VEX ^C4 ^E1 ^FC OPCODE ;
:   VEX.256.66.0F.W1 CALL> REX/VEX ^C4 ^E1 ^FD OPCODE ;
:   VEX.256.F3.0F.W1 CALL> REX/VEX ^C4 ^E1 ^FE OPCODE ;
:   VEX.256.F2.0F.W1 CALL> REX/VEX ^C4 ^E1 ^FF OPCODE ;
:    VEX.128.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^F8 OPCODE ;
: VEX.128.66.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^F9 OPCODE ;
: VEX.128.F3.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^FA OPCODE ;
: VEX.128.F2.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^FB OPCODE ;
:    VEX.256.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^FC OPCODE ;
: VEX.256.66.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^FD OPCODE ;
: VEX.256.F3.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^FE OPCODE ;
: VEX.256.F2.0F38.W1 CALL> REX/VEX ^C4 ^E2 ^FF OPCODE ;
:    VEX.128.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^F8 OPCODE ;
: VEX.128.66.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^F9 OPCODE ;
: VEX.128.F3.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^FA OPCODE ;
: VEX.128.F2.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^FB OPCODE ;
:    VEX.256.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^FC OPCODE ;
: VEX.256.66.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^FD OPCODE ;
: VEX.256.F3.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^FE OPCODE ;
: VEX.256.F2.0F3A.W1 CALL> REX/VEX ^C4 ^E3 ^FF OPCODE ;

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
: FS: ^64 ; : GS: ^65 ;

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
: RDPMC   OPL ^0F ^33 ;

: PUSHFS  OPL ^0F ^A0 ;
: POPFS   OPL ^0F ^A1 ;
: CPUID   OPL ^0F ^A2 ;
: PUSHGS  OPL ^0F ^A8 ;
: POPGS   OPL ^0F ^A9 ;

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

: LEAW< OPW ^8D ;
: LEAL< OPL ^8D ;
: LEAQ< OPQ ^8D ;

:   BTW> OPW ^0F ^A3 ; :   BTL> OPL ^0F ^A3 ; :   BTQ> OPQ ^0F ^A3 ;
: SHLDW> OPW ^0F ^A5 ; : SHLDL> OPL ^0F ^A5 ; : SHLDQ> OPQ ^0F ^A5 ;
:  BTSW> OPW ^0F ^AB ; :  BTSL> OPL ^0F ^AB ; :  BTSQ> OPQ ^0F ^AB ;
: SHRDW> OPW ^0F ^AD ; : SHRDL> OPL ^0F ^AD ; : SHRDQ> OPQ ^0F ^AD ;
: IMULW< OPW ^0F ^AF ; : IMULL< OPL ^0F ^AF ; : IMULQ< OPQ ^0F ^AF ;

: CMPXCHGB> OPB ^0F ^B0 ;
: CMPXCHGW> OPW ^0F ^B1 ;
: CMPXCHGL> OPL ^0F ^B1 ;
: CMPXCHGQ> OPQ ^0F ^B1 ;

:  BTRW> OPW ^0F ^B3 ; :  BTRL> OPL ^0F ^B3 ; :  BTRQ> OPQ ^0F ^B3 ;
:  BTCW> OPW ^0F ^BB ; :  BTCL> OPL ^0F ^BB ; :  BTCQ> OPQ ^0F ^BB ;

: POPCNTW< ^F3 OPW ^0F ^B8 ;
: POPCNTL< ^F3 OPL ^0F ^B8 ;
: POPCNTQ< ^F3 OPQ ^0F ^B8 ;

: BSFW< OPW ^0F ^BC ; : BSFL< OPL ^0F ^BC ; : BSFQ< OPQ ^0F ^BC ;
: BSRW< OPW ^0F ^BD ; : BSRL< OPL ^0F ^BD ; : BSRQ< OPQ ^0F ^BD ;

: TZCNTW< ^F3 OPW ^0F ^BC ; : LZCNTW< ^F3 OPW ^0F ^BD ;
: TZCNTL< ^F3 OPL ^0F ^BC ; : LZCNTL< ^F3 OPL ^0F ^BD ;
: TZCNTQ< ^F3 OPQ ^0F ^BC ; : LZCNTQ< ^F3 OPQ ^0F ^BD ;

: MOVZXWB< OPW ^0F ^B6 ; : MOVSXWB< OPW ^0F ^BE ;
: MOVZXLB< OPL ^0F ^B6 ; : MOVSXLB< OPL ^0F ^BE ;
: MOVZXQB< OPQ ^0F ^B6 ; : MOVSXQB< OPQ ^0F ^BE ;
: MOVZXLW< OPL ^0F ^B7 ; : MOVSXLW< OPL ^0F ^BF ;
: MOVZXQW< OPQ ^0F ^B7 ; : MOVSXQW< OPQ ^0F ^BF ;
( -------------------- ) : MOVSXQL< OPQ ^63     ;

: MOVNTIL> OPL ^0F ^C3 ;
: MOVNTIQ> OPQ ^0F ^C3 ;

\ these need a memory operand
: MOVBEW< OPW ^0F ^38 ^F0 ; : MOVBEW> OPW ^0F ^38 ^F1 ;
: MOVBEL< OPL ^0F ^38 ^F0 ; : MOVBEL> OPL ^0F ^38 ^F1 ;
: MOVBEQ< OPQ ^0F ^39 ^F0 ; : MOVBEQ> OPQ ^0F ^39 ^F1 ;

: CRC32B< ^F2 OPB ^0F ^38 ^F0 ;
: CRC32W< ^F2 OPW ^0F ^38 ^F1 ;
: CRC32L< ^F2 OPL ^0F ^38 ^F1 ;
: CRC32Q< ^F2 OPQ ^0F ^38 ^F1 ;

: ADCXL< ^66 OPL ^0F ^38 ^F6 ;
: ADCXQ< ^66 OPQ ^0F ^38 ^F6 ;
: ADOXL< ^F3 OPL ^0F ^38 ^F6 ;
: ADOXQ< ^F3 OPQ ^0F ^38 ^F6 ;

\ Ops that expect register, modrm, and immediate arguments.
\ (Do not write a comma after the modrm argument!)
\ E.g.  IMULW<. RAX, RCX $0A
: IMULW<. OPW ^6B ; : IMULW<.. OPW ^69 ;
: IMULL<. OPL ^6B ; : IMULL<:  OPL ^69 ;
: IMULQ<. OPQ ^6B ; : IMULQ<:  OPQ ^69 ;

: SHLDW>. OPW ^0F ^A4 ; : SHLDL>. OPL ^0F ^A4 ; : SHLDQ>. OPQ ^0F ^A4 ;
: SHRDW>. OPW ^0F ^AC ; : SHRDL>. OPL ^0F ^AC ; : SHRDQ>. OPQ ^0F ^AC ;

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
: PUSHQ.  OPL ^6A ; \ Pushes 64-bits even without REX.W prefix
: PUSHQ:  OPL ^68 ; \ Pushes 64-bits even without REX.W prefix

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

: BSWAPL# OPL ^0F ^C8 ;
: BSWAPQ# OPQ ^0F ^C8 ;

: RDFSBASEL# ^F3 OPL ^0F ^AE /0 ;
: RDFSBASEQ# ^F3 OPQ ^0F ^AE /0 ;
: RDGSBASEL# ^F3 OPL ^0F ^AE /1 ;
: RDGSBASEQ# ^F3 OPQ ^0F ^AE /1 ;
: WRFSBASEL# ^F3 OPL ^0F ^AE /2 ;
: WRFSBASEQ# ^F3 OPQ ^0F ^AE /2 ;
: WRGSBASEL# ^F3 OPL ^0F ^AE /3 ;
: WRGSBASEQ# ^F3 OPQ ^0F ^AE /3 ;

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

: SETO_  OPB ^0F ^90 /0 ; : SETNO_ OPB ^0F ^91 /0 ;
: SETB_  OPB ^0F ^92 /0 ; : SETAE_ OPB ^0F ^93 /0 ;
: SETZ_  OPB ^0F ^94 /0 ; : SETNZ_ OPB ^0F ^95 /0 ;
: SETBE_ OPB ^0F ^96 /0 ; : SETA_  OPB ^0F ^97 /0 ;
: SETS_  OPB ^0F ^98 /0 ; : SETNS_ OPB ^0F ^99 /0 ;
: SETPE_ OPB ^0F ^9A /0 ; : SETPO_ OPB ^0F ^9B /0 ;
: SETL_  OPB ^0F ^9C /0 ; : SETGE_ OPB ^0F ^9D /0 ;
: SETLE_ OPB ^0F ^9E /0 ; : SETG_  OPB ^0F ^9F /0 ;

: CMPXCHG8B_  OPL ^0F ^C7 /1 ; \ must take memory operand
: CMPXCHG16B_ OPQ ^0F ^C7 /1 ; \ must take memory operand

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

:  BTW_. OPW ^0F ^BA /4 ; :  BTL_. OPL ^0F ^BA /4 ; :  BTQ_. OPQ ^0F ^BA /4 ;
: BTSW_. OPW ^0F ^BA /5 ; : BTSL_. OPL ^0F ^BA /5 ; : BTSQ_. OPQ ^0F ^BA /5 ;
: BTRW_. OPW ^0F ^BA /6 ; : BTRL_. OPL ^0F ^BA /6 ; : BTRQ_. OPQ ^0F ^BA /6 ;
: BTCW_. OPW ^0F ^BA /7 ; : BTCL_. OPL ^0F ^BA /7 ; : BTCQ_. OPQ ^0F ^BA /7 ;

\ SIMD

:  MOVUPSX<       OPL ^0F ^10 ; :  MOVUPSX>         OPL ^0F ^11 ;
:  MOVUPDX<   ^66 OPL ^0F ^10 ; :  MOVUPDX>     ^66 OPL ^0F ^11 ;
: VMOVUPSX<    VEX.128.0F ^10 ; : VMOVUPSX>      VEX.128.0F ^11 ;
: VMOVUPSY<    VEX.256.0F ^10 ; : VMOVUPSY>      VEX.256.0F ^11 ;
: VMOVUPDX< VEX.128.66.0F ^10 ; : VMOVUPDX>   VEX.128.66.0F ^11 ;
: VMOVUPDY< VEX.256.66.0F ^10 ; : VMOVUPDY>   VEX.256.66.0F ^11 ;
:   MOVSSX<   ^F3 OPL ^0F ^10 ; :   MOVSSX>     ^F3 OPL ^0F ^11 ;
:   MOVSDX<   ^F2 OPL ^0F ^10 ; :   MOVSDX>     ^F2 OPL ^0F ^11 ;
: VMOVSSXM< VEX.128.F3.0F ^10 ; : VMOVSSXM>  VEX.128.F3.0F ^11 ;
: VMOVSSXX+ VEX.128.F3.0F ^10 ; : VMOVSSXX+> VEX.128.F3.0F ^11 ;
: VMOVSDXM< VEX.128.F2.0F ^10 ; : VMOVSDXM>  VEX.128.F2.0F ^11 ;
: VMOVSDXX+ VEX.128.F2.0F ^10 ; : VMOVSDXX+> VEX.128.F2.0F ^11 ;

:   MOVLPSXM<        OPL ^0F ^12 ; :  MOVLPSXM>       OPL ^0F ^13 ;
:   MOVLPDXM<    ^66 OPL ^0F ^12 ; :  MOVLPDXM>   ^66 OPL ^0F ^13 ;
:  VMOVLPSXM+     VEX.128.0F ^12 ; : VMOVLPSXM>    VEX.128.0F ^13 ;
:  VMOVLPDXM+  VEX.128.66.0F ^12 ; : VMOVLPDXM> VEX.128.66.0F ^13 ;
:  MOVHLPSXX<        OPL ^0F ^12 ;
: VMOVHLPSXX+     VEX.128.0F ^12 ;
:  MOVSLDUPX<    ^F3 OPL ^0F ^12 ;
: VMOVSLDUPX<  VEX.128.F3.0F ^12 ;
: VMOVSLDUPY<  VEX.256.F3.0F ^12 ;
:   MOVDDUPX<    ^F2 OPL ^0F ^12 ;
:  VMOVDDUPX<  VEX.128.F2.0F ^12 ;
:  VMOVDDUPY<  VEX.256.F2.0F ^12 ;

:   UNPCKLPS<       OPL ^0F ^14 ; :   UNPCKHPS<       OPL ^0F ^15 ;
:   UNPCKLPD<   ^66 OPL ^0F ^14 ; :   UNPCKHPD<   ^66 OPL ^0F ^15 ;
: VUNPCKLPSX+    VEX.128.0F ^14 ; : VUNPCKHPSX+    VEX.128.0F ^15 ;
: VUNPCKLPSY+    VEX.256.0F ^14 ; : VUNPCKHPSY+    VEX.256.0F ^15 ;
: VUNPCKLPDX+ VEX.128.66.0F ^14 ; : VUNPCKHPDX+ VEX.128.66.0F ^15 ;
: VUNPCKLPDY+ VEX.256.66.0F ^14 ; : VUNPCKHPDY+ VEX.256.66.0F ^15 ;

:   MOVHPSXM<        OPL ^0F ^16 ; :  MOVHPSXM>       OPL ^0F ^17 ;
:   MOVHPDXM<    ^66 OPL ^0F ^16 ; :  MOVHPDXM>   ^66 OPL ^0F ^17 ;
:  VMOVHPSXM+     VEX.128.0F ^16 ; : VMOVHPSXM>    VEX.128.0F ^17 ;
:  VMOVHPDXM+  VEX.128.66.0F ^16 ; : VMOVHPDXM> VEX.128.66.0F ^17 ;
:  MOVLHPSXX<        OPL ^0F ^16 ;
: VMOVLHPSXX+     VEX.128.0F ^16 ;
:  MOVSHDUPX<    ^F3 OPL ^0F ^16 ;
: VMOVSHDUPX<  VEX.128.F3.0F ^16 ;
: VMOVSHDUPY<  VEX.256.F3.0F ^16 ;

:  MOVAPSX<       OPL ^0F ^28 ; :  MOVAPSX>       OPL ^0F ^29 ;
:  MOVAPDX<   ^66 OPL ^0F ^28 ; :  MOVAPDX>   ^66 OPL ^0F ^29 ;
: VMOVAPSX<    VEX.128.0F ^28 ; : VMOVAPSX>    VEX.128.0F ^29 ;
: VMOVAPDX< VEX.128.66.0F ^28 ; : VMOVAPDX> VEX.128.66.0F ^29 ;
: VMOVAPSY<    VEX.256.0F ^28 ; : VMOVAPSY>    VEX.256.0F ^29 ;
: VMOVAPDY< VEX.256.66.0F ^28 ; : VMOVAPDY> VEX.256.66.0F ^29 ;

:  CVTPI2PSXM<       OPL ^0F    ^2A ; :  MOVNTPSXM>    OPL ^0F ^2B ;
:  CVTPI2PDXM<   ^66 OPL ^0F    ^2A ; : VMOVNTPSXM> VEX.128.0F ^2B ;
:  CVTSI2SSXL<   ^F3 OPL ^0F    ^2A ; : VMOVNTPSYM> VEX.256.0F ^2B ;
:  CVTSI2SSXQ<   ^F3 OPQ ^0F    ^2A ;
:  CVTSI2SDXL<   ^F2 OPL ^0F    ^2A ;
:  CVTSI2SDXQ<   ^F2 OPQ ^0F    ^2A ;
: VCVTSI2SSXL+ VEX.128.F3.0F    ^2A ;
: VCVTSI2SSXQ+ VEX.128.F3.0F.W1 ^2A ;
: VCVTSI2SDXL+ VEX.128.F2.0F    ^2A ;
: VCVTSI2SDXQ+ VEX.128.F2.0F.W1 ^2A ;

:  CVTTSS2SILX<   ^F3 OPL ^0F    ^2C ; :  CVTSS2SILX<   ^F3 OPL ^0F    ^2D ;
:  CVTTSS2SIQX<   ^F3 OPQ ^0F    ^2C ; :  CVTSS2SIQX<   ^F3 OPQ ^0F    ^2D ;
:  CVTTSD2SILX<   ^F2 OPL ^0F    ^2C ; :  CVTSD2SILX<   ^F2 OPL ^0F    ^2D ;
:  CVTTSD2SIQX<   ^F2 OPQ ^0F    ^2C ; :  CVTSD2SIQX<   ^F2 OPQ ^0F    ^2D ;
: VCVTTSS2SILX< VEX.128.F3.0F    ^2C ; : VCVTSS2SILX< VEX.128.F3.0F    ^2D ;
: VCVTTSS2SIQX< VEX.128.F3.0F.W1 ^2C ; : VCVTSS2SIQX< VEX.128.F3.0F.W1 ^2D ;
: VCVTTSD2SILX< VEX.128.F2.0F    ^2C ; : VCVTSD2SILX< VEX.128.F2.0F    ^2D ;
: VCVTTSD2SIQX< VEX.128.F2.0F.W1 ^2C ; : VCVTSD2SIQX< VEX.128.F2.0F.W1 ^2D ;

:  UCOMISSX<       OPL ^0F ^2E ; :  COMISSX<       OPL ^0F ^2F ;
:  UCOMISDX<   ^66 OPL ^0F ^2E ; :  COMISDX<   ^66 OPL ^0F ^2F ;
: VUCOMISSX<    VEX.128.0F ^2E ; : VCOMISSX<    VEX.128.0F ^2F ;
: VUCOMISDX< VEX.128.66.0F ^2E ; : VCOMISDX< VEX.128.66.0F ^2F ;

:  MOVMSKPSX<       OPL ^0F ^50 ; :  SQRTPSX<       OPL ^0F ^51 ;
:  MOVMSKPDX<   ^66 OPL ^0F ^50 ; :  SQRTPDX<   ^66 OPL ^0F ^51 ;
: VMOVMSKPSX<    VEX.128.0F ^50 ; : VSQRTPSX<    VEX.128.0F ^51 ;
: VMOVMSKPSY<    VEX.256.0F ^50 ; : VSQRTPSY<    VEX.256.0F ^51 ;
: VMOVMSKPDX< VEX.128.66.0F ^50 ; : VSQRTPDX< VEX.128.66.0F ^51 ;
: VMOVMSKPDY< VEX.256.66.0F ^50 ; : VSQRTPDY< VEX.256.66.0F ^51 ;
                                  :  SQRTSSX<   ^F3 OPL ^0F ^51 ;
                                  :  SQRTSDX<   ^F2 OPL ^0F ^51 ;
                                  : VSQRTSSX+ VEX.128.F3.0F ^51 ;
                                  : VSQRTSDX+ VEX.128.F2.0F ^51 ;

:  RSQRTPSX<       OPL ^0F ^52 ; :  RCPPSX<       OPL ^0F ^53 ;
: VRSQRTPSX<    VEX.128.0F ^52 ; : VRCPPSX<    VEX.128.0F ^53 ;
: VRSQRTPSY<    VEX.256.0F ^52 ; : VRCPPSY<    VEX.256.0F ^53 ;
:  RSQRTSSX<   ^F3 OPL ^0F ^52 ; :  RCPSSX<   ^F3 OPL ^0F ^53 ;
: VRSQRTSSX+ VEX.128.F3.0F ^52 ; : VRCPSSX+ VEX.128.F3.0F ^53 ;

:  ANDPSX<       OPL ^0F ^54 ; :  ANDNPSX<       OPL ^0F ^55 ;
:  ANDPDX<   ^66 OPL ^0F ^54 ; :  ANDNPDX<   ^66 OPL ^0F ^55 ;
: VANDPSX+    VEX.128.0F ^54 ; : VANDNPSX+    VEX.128.0F ^55 ;
: VANDPDX+ VEX.128.66.0F ^54 ; : VANDNPDX+ VEX.128.66.0F ^55 ;
: VANDPSY+    VEX.256.0F ^54 ; : VANDNPSY+    VEX.256.0F ^55 ;
: VANDPDY+ VEX.256.66.0F ^54 ; : VANDNPDY+ VEX.256.66.0F ^55 ;

:  ORPSX<       OPL ^0F ^56 ; :  XORPSX<       OPL ^0F ^57 ;
:  ORPDX<   ^66 OPL ^0F ^56 ; :  XORPDX<   ^66 OPL ^0F ^57 ;
: VORPSX+    VEX.128.0F ^56 ; : VXORPSX+    VEX.128.0F ^57 ;
: VORPDX+ VEX.128.66.0F ^56 ; : VXORPDX+ VEX.128.66.0F ^57 ;
: VORPSY+    VEX.256.0F ^56 ; : VXORPSY+    VEX.256.0F ^57 ;
: VORPDY+ VEX.256.66.0F ^56 ; : VXORPDY+ VEX.256.66.0F ^57 ;

:  ADDPSX<       OPL ^0F ^58 ; :  MULPSX<       OPL ^0F ^59 ;
:  ADDPDX<   ^66 OPL ^0F ^58 ; :  MULPDX<   ^66 OPL ^0F ^59 ;
: VADDPSX+    VEX.128.0F ^58 ; : VMULPSX+    VEX.128.0F ^59 ;
: VADDPSY+    VEX.256.0F ^58 ; : VMULPSY+    VEX.256.0F ^59 ;
: VADDPDX+ VEX.128.66.0F ^58 ; : VMULPDX+ VEX.128.66.0F ^59 ;
: VADDPDY+ VEX.256.66.0F ^58 ; : VMULPDY+ VEX.256.66.0F ^59 ;
:  ADDSSX<   ^F3 OPL ^0F ^58 ; :  MULSSX<   ^F3 OPL ^0F ^59 ;
:  ADDSDX<   ^F2 OPL ^0F ^58 ; :  MULSDX<   ^F2 OPL ^0F ^59 ;
: VADDSSX+ VEX.128.F3.0F ^58 ; : VMULSSX+ VEX.128.F3.0F ^59 ;
: VADDSDX+ VEX.128.F2.0F ^58 ; : VMULSDX+ VEX.128.F2.0F ^59 ;

:  CVTPS2PDX<       OPL ^0F ^5A ; :   CVTDQ2PSX<       OPL ^0F ^5B ;
:  CVTPD2PSX<   ^66 OPL ^0F ^5A ; :   CVTPS2DQX<   ^66 OPL ^0F ^5B ;
: VCVTPS2PDX<    VEX.128.0F ^5A ; :  CVTTPS2DQX<   ^F3 OPL ^0F ^5B ;
: VCVTPS2PDY<    VEX.256.0F ^5A ; :  VCVTDQ2PSX<    VEX.128.0F ^5B ;
: VCVTPD2PSX< VEX.128.66.0F ^5A ; :  VCVTDQ2PSY<    VEX.256.0F ^5B ;
: VCVTPD2PSY< VEX.256.66.0F ^5A ; :  VCVTPS2DQX< VEX.128.66.0F ^5B ;
:  CVTSS2SDX<   ^F3 OPL ^0F ^5A ; :  VCVTPS2DQY< VEX.256.66.0F ^5B ;
:  CVTSD2SSX<   ^F2 OPL ^0F ^5A ; : VCVTTPS2DQX< VEX.128.F3.0F ^5B ;
: VCVTSS2SDX+ VEX.128.F3.0F ^5A ; : VCVTTPS2DQY< VEX.128.F3.0F ^5B ;
: VCVTSD2SSX+ VEX.128.F2.0F ^5A ;

:  SUBPSX<       OPL ^0F ^5C ; :  MINPSX<       OPL ^0F ^5D ;
:  SUBPDX<   ^66 OPL ^0F ^5C ; :  MINPDX<   ^66 OPL ^0F ^5D ;
: VSUBPSX+    VEX.128.0F ^5C ; : VMINPSX+    VEX.128.0F ^5D ;
: VSUBPSY+    VEX.256.0F ^5C ; : VMINPSY+    VEX.256.0F ^5D ;
: VSUBPDX+ VEX.128.66.0F ^5C ; : VMINPDX+ VEX.128.66.0F ^5D ;
: VSUBPDY+ VEX.256.66.0F ^5C ; : VMINPDY+ VEX.256.66.0F ^5D ;
:  SUBSSX<   ^F3 OPL ^0F ^5C ; :  MINSSX<   ^F3 OPL ^0F ^5D ;
:  SUBSDX<   ^F2 OPL ^0F ^5C ; :  MINSDX<   ^F2 OPL ^0F ^5D ;
: VSUBSSX+ VEX.128.F3.0F ^5C ; : VMINSSX+ VEX.128.F3.0F ^5D ;
: VSUBSDX+ VEX.128.F2.0F ^5C ; : VMINSDX+ VEX.128.F2.0F ^5D ;

:  DIVPSX<       OPL ^0F ^5E ; :  MAXPSX<       OPL ^0F ^5F ;
:  DIVPDX<   ^66 OPL ^0F ^5E ; :  MAXPDX<   ^66 OPL ^0F ^5F ;
: VDIVPSX+    VEX.128.0F ^5E ; : VMAXPSX+    VEX.128.0F ^5F ;
: VDIVPSY+    VEX.256.0F ^5E ; : VMAXPSY+    VEX.256.0F ^5F ;
: VDIVPDX+ VEX.128.66.0F ^5E ; : VMAXPDX+ VEX.128.66.0F ^5F ;
: VDIVPDY+ VEX.256.66.0F ^5E ; : VMAXPDY+ VEX.256.66.0F ^5F ;
:  DIVSSX<   ^F3 OPL ^0F ^5E ; :  MAXSSX<   ^F3 OPL ^0F ^5F ;
:  DIVSDX<   ^F2 OPL ^0F ^5E ; :  MAXSDX<   ^F2 OPL ^0F ^5F ;
: VDIVSSX+ VEX.128.F3.0F ^5E ; : VMAXSSX+ VEX.128.F3.0F ^5F ;
: VDIVSDX+ VEX.128.F2.0F ^5E ; : VMAXSDX+ VEX.128.F2.0F ^5F ;

:  PUNPCKLBWX<   ^66 OPL ^0F ^60 ; :  PUNPCKLWDX<   ^66 OPL ^0F ^61 ;
: VPUNPCKLBWX+ VEX.128.66.0F ^60 ; : VPUNPCKLWDX+ VEX.128.66.0F ^61 ;
: VPUNPCKLBWY+ VEX.256.66.0F ^60 ; : VPUNPCKLWDY+ VEX.256.66.0F ^61 ;
:  PUNPCKLDQX<   ^66 OPL ^0F ^62 ; :   PACKSSWBX<   ^66 OPL ^0F ^63 ;
: VPUNPCKLDQX+ VEX.128.66.0F ^62 ; :  VPACKSSWBX+ VEX.128.66.0F ^63 ;
: VPUNPCKLDQY+ VEX.256.66.0F ^62 ; :  VPACKSSWBY+ VEX.256.66.0F ^63 ;
:    PCMPGTBX<   ^66 OPL ^0F ^64 ; :    PCMPGTWX<   ^66 OPL ^0F ^65 ;
:   VPCMPGTBX+ VEX.128.66.0F ^64 ; :   VPCMPGTWX+ VEX.128.66.0F ^65 ;
:   VPCMPGTBY+ VEX.256.66.0F ^64 ; :   VPCMPGTWY+ VEX.256.66.0F ^65 ;
:    PCMPGTDX<   ^66 OPL ^0F ^66 ; :   PACKUSWBX<   ^66 OPL ^0F ^67 ;
:   VPCMPGTDX+ VEX.128.66.0F ^66 ; :  VPACKUSWBX+ VEX.128.66.0F ^67 ;
:   VPCMPGTDY+ VEX.256.66.0F ^66 ; :  VPACKUSWBY+ VEX.256.66.0F ^67 ;

:   PUNPCKHBWX<   ^66 OPL ^0F ^68 ; :   PUNPCKHWDX<   ^66 OPL ^0F ^69 ;
:  VPUNPCKHBWX+ VEX.128.66.0F ^68 ; :  VPUNPCKHWDX+ VEX.128.66.0F ^69 ;
:  VPUNPCKHBWY+ VEX.256.66.0F ^68 ; :  VPUNPCKHWDY+ VEX.256.66.0F ^69 ;
:   PUNPCKHDQX<   ^66 OPL ^0F ^6A ; :    PACKSSDWX<   ^66 OPL ^0F ^6B ;
:  VPUNPCKHDQX+ VEX.128.66.0F ^6A ; :   VPACKSSDWX+ VEX.128.66.0F ^6B ;
:  VPUNPCKHDQY+ VEX.256.66.0F ^6A ; :   VPACKSSDWY+ VEX.256.66.0F ^6B ;
:  PUNPCKLQDQX<   ^66 OPL ^0F ^6C ; :  PUNPCKHQDQX<   ^66 OPL ^0F ^6D ;
: VPUNPCKLQDQX+ VEX.128.66.0F ^6C ; : VPUNPCKHQDQX< VEX.128.66.0F ^6D ;
: VPUNPCKLQDQY+ VEX.256.66.0F ^6C ; : VPUNPCKHQDQY< VEX.256.66.0F ^6D ;

:  MOVXL<   ^66 OPL ^0F    ^6E ; :  MOVDQAX<   ^66 OPL ^0F ^6F ;
:  MOVXQ<   ^66 OPQ ^0F    ^6E ; : VMOVDQAX< VEX.128.66.0F ^6F ;
: VMOVXL< VEX.128.66.0F    ^6E ; : VMOVDQAY< VEX.256.66.0F ^6F ;
: VMOVYL< VEX.256.66.0F    ^6E ; :  MOVDQUX<   ^F3 OPL ^0F ^6F ;
: VMOVXQ< VEX.128.66.0F.W1 ^6E ; : VMOVDQUX< VEX.128.F3.0F ^6F ;
: VMOVYQ< VEX.128.66.0F.W1 ^6E ; : VMOVDQUY< VEX.256.F3.0F ^6F ;

:   PSHUFDX<.   ^66 OPL ^0F ^70 ;
:  VPSHUFDX<. VEX.128.66.0F ^70 ;
:  VPSHUFDY<. VEX.256.66.0F ^70 ;
:  PSHUFHWX<.   ^F3 OPL ^0F ^70 ;
: VPSHUFHWX<. VEX.128.F3.0F ^70 ;
: VPSHUFHWY<. VEX.256.F3.0F ^70 ;
:  PSHUFLWX<.   ^F2 OPL ^0F ^70 ;
: VPSHUFLWX<. VEX.128.F2.0F ^70 ;
: VPSHUFLWY<. VEX.256.F2.0F ^70 ;

:  PSRLWX#.    ^66 OPL ^0F ^71 /2 ; :  PSRLDX#.    ^66 OPL ^0F ^72 /2 ;
: VPSRLWX+#. VEX.128.66.0F ^71 /2 ; : VPSRLDX+#. VEX.128.66.0F ^72 /2 ;
: VPSRLWY+#. VEX.256.66.0F ^71 /2 ; : VPSRLDY+#. VEX.256.66.0F ^72 /2 ;
:  PSRAWX#.    ^66 OPL ^0F ^71 /4 ; :  PSRADX#.    ^66 OPL ^0F ^72 /4 ;
: VPSRAWX+#. VEX.128.66.0F ^71 /4 ; : VPSRADX+#. VEX.128.66.0F ^72 /4 ;
: VPSRAWY+#. VEX.256.66.0F ^71 /4 ; : VPSRADY+#. VEX.256.66.0F ^72 /4 ;
:  PSLLWX#.    ^66 OPL ^0F ^71 /6 ; :  PSLLDX#.    ^66 OPL ^0F ^72 /6 ;
: VPSLLWX+#. VEX.128.66.0F ^71 /6 ; : VPSLLDX+#. VEX.128.66.0F ^72 /6 ;
: VPSLLWY+#. VEX.256.66.0F ^71 /6 ; : VPSLLDY+#. VEX.256.66.0F ^72 /6 ;

:   PSRLQX#.    ^66 OPL ^0F ^73 /2 ;
:  VPSRLQX+#. VEX.128.66.0F ^73 /2 ;
:  VPSRLQY+#. VEX.256.66.0F ^73 /2 ;
:  PSRLDQX#.    ^66 OPL ^0F ^73 /3 ;
: VPSRLDQX+#. VEX.128.66.0F ^73 /3 ;
: VPSRLDQY+#. VEX.256.66.0F ^73 /3 ;
:   PSLLQX#.    ^66 OPL ^0F ^73 /6 ;
:  VPSLLQX+#. VEX.128.66.0F ^73 /6 ;
:  VPSLLQY+#. VEX.256.66.0F ^73 /6 ;
:  PSLLDQX#.    ^66 OPL ^0F ^73 /7 ;
: VPSLLDQX+#. VEX.128.66.0F ^73 /7 ;
: VPSLLDQY+#. VEX.256.66.0F ^73 /7 ;

:  PCMPEQBX<   ^66 OPL ^0F ^74 ; :  PCMPEQWX<   ^66 OPL ^0F ^75 ;
: VPCMPEQBX+ VEX.128.66.0F ^74 ; : VPCMPEQWX+ VEX.128.66.0F ^75 ;
: VPCMPEQBY+ VEX.256.66.0F ^74 ; : VPCMPEQWY+ VEX.256.66.0F ^75 ;
:  PCMPEQDX<   ^66 OPL ^0F ^76 ;
: VPCMPEQDX+ VEX.128.66.0F ^76 ; : VZEROUPPER VEX.128.0F ^77 ;
: VPCMPEQDY+ VEX.256.66.0F ^76 ; : VZEROALL   VEX.256.0F ^77 ;

:  HADDPDX<   ^66 OPL ^0F ^7C ; :  HSUBPDX<   ^66 OPL ^0F ^7D ;
: VHADDPDX+ VEX.128.66.0F ^7C ; : VHSUBPDX+ VEX.128.66.0F ^7D ;
: VHADDPDY+ VEX.256.66.0F ^7C ; : VHSUBPDY+ VEX.256.66.0F ^7D ;
:  HADDPSX<   ^F2 OPL ^0F ^7C ; :  HSUBPSX<   ^F2 OPL ^0F ^7D ;
: VHADDPSX+ VEX.128.F2.0F ^7C ; : VHSUBPSX+ VEX.128.F2.0F ^7D ;
: VHADDPSY+ VEX.256.F2.0F ^7C ; : VHSUBPSY+ VEX.256.F2.0F ^7D ;

:   MOVXL>   ^66 OPL ^0F    ^7E ; :  MOVDQAX>   ^66 OPL ^0F ^7F ;
:   MOVXQ>   ^66 OPQ ^0F    ^7E ; : VMOVDQAX> VEX.128.66.0F ^7F ;
:  VMOVXL> VEX.128.66.0F    ^7E ; : VMOVDQAY> VEX.256.66.0F ^7F ;
:  VMOVYL> VEX.256.66.0F    ^7E ; :  MOVDQUX>   ^F3 OPL ^0F ^7F ;
:  VMOVXQ> VEX.128.66.0F.W1 ^7E ; : VMOVDQUX> VEX.128.F3.0F ^7F ;
:  VMOVYQ> VEX.128.66.0F.W1 ^7E ; : VMOVDQUY> VEX.256.F3.0F ^7F ;
:  MOVQXX<   ^F3 OPL ^0F    ^7E ;
: VMOVQXX< VEX.128.F3.0F    ^7E ;

:  CMPPSX<.       OPL ^0F ^C2 ;
: VCMPPSX+.    VEX.128.0F ^C2 ;
: VCMPPSY+.    VEX.256.0F ^C2 ;
:  CMPPDX<.   ^66 OPL ^0F ^C2 ;
: VCMPPDX+. VEX.128.66.0F ^C2 ;
: VCMPPDY+. VEX.256.66.0F ^C2 ;
:  CMPSSX<.   ^F3 OPL ^0F ^C2 ;
: VCMPSSX+. VEX.128.F3.0F ^C2 ;
:  CMPSDX<.   ^F2 OPL ^0F ^C2 ;
: VCMPSDX+. VEX.128.F2.0F ^C2 ;

:  PINSRXW<.   ^66 OPL ^0F ^C4 ; :  PEXTRWX<.   ^66 OPL ^0F ^C5 ;
: VPINSRXW+. VEX.128.66.0F ^C4 ; : VPEXTRWX<. VEX.128.66.0F ^C5 ;

:  SHUFPSX<.       OPL ^0F ^C6 ;
: VSHUFPSX+.    VEX.128.0F ^C6 ;
: VSHUFPSY+.    VEX.256.0F ^C6 ;
:  SHUFPDX<.   ^66 OPL ^0F ^C6 ;
: VSHUFPDX+. VEX.128.66.0F ^C6 ;
: VSHUFPDY+. VEX.256.66.0F ^C6 ;

:  ADDSUBPDX<   ^66 OPL ^0F ^D0 ; :  PSRLWX<   ^66 OPL ^0F ^D1 ;
: VADDSUBPDX+ VEX.128.66.0F ^D0 ; : VPSRLWX+ VEX.128.66.0F ^D1 ;
: VADDSUBPDY+ VEX.256.66.0F ^D0 ; : VPSRLWY+ VEX.256.66.0F ^D1 ;
:  ADDSUBPSX<   ^F2 OPL ^0F ^D0 ;
: VADDSUBPSX+ VEX.128.F2.0F ^D0 ;
: VADDSUBPSY+ VEX.256.F2.0F ^D0 ;

:  PSRLDX<   ^66 OPL ^0F ^D2 ; :  PSRLQX<   ^66 OPL ^0F ^D3 ;
: VPSRLDX+ VEX.128.66.0F ^D2 ; : VPSRLQX+ VEX.128.66.0F ^D3 ;
: VPSRLDY+ VEX.256.66.0F ^D2 ; : VPSRLQY+ VEX.256.66.0F ^D3 ;

:  PADDQX<   ^66 OPL ^0F ^D4 ; :  PMULLWX<   ^66 OPL ^0F ^D5 ;
: VPADDQX+ VEX.128.66.0F ^D4 ; : VPMULLWX+ VEX.128.66.0F ^D5 ;
: VPADDQY+ VEX.256.66.0F ^D4 ; : VPMULLWY+ VEX.256.66.0F ^D5 ;

:  MOVQXX>   ^66 OPL ^0F ^D6 ; :  PMOVMSKBX<   ^66 OPL ^0F ^D7 ;
: VMOVQXX> VEX.128.66.0F ^D6 ; : VPMOVMSKBX< VEX.128.66.0F ^D7 ;
                               : VPMOVMSKBY< VEX.256.66.0F ^D7 ;

:  PSUBUSBX<   ^66 OPL ^0F ^D8 ; :  PSUBUSWX<   ^66 OPL ^0F ^D9 ;
: VPSUBUSBX+ VEX.128.66.0F ^D8 ; : VPSUBUSWX+ VEX.128.66.0F ^D9 ;
: VPSUBUSBY+ VEX.256.66.0F ^D8 ; : VPSUBUSWY+ VEX.256.66.0F ^D9 ;

:  PMINUBX<   ^66 OPL ^0F ^DA ; :  PANDX<   ^66 OPL ^0F ^DB ;
: VPMINUBX+ VEX.128.66.0F ^DA ; : VPANDX+ VEX.128.66.0F ^DB ;
: VPMINUBY+ VEX.256.66.0F ^DA ; : VPANDY+ VEX.256.66.0F ^DB ;

:  PADDUSBX<   ^66 OPL ^0F ^DC ; :  PADDUSWX<   ^66 OPL ^0F ^DD ;
: VPADDUSBX+ VEX.128.66.0F ^DC ; : VPADDUSWX+ VEX.128.66.0F ^DD ;
: VPADDUSBY+ VEX.256.66.0F ^DC ; : VPADDUSWY+ VEX.256.66.0F ^DD ;

:  PMAXUBX<   ^66 OPL ^0F ^DE ; :  PANDNX<   ^66 OPL ^0F ^DF ;
: VPMAXUBX+ VEX.128.66.0F ^DE ; : VPANDNX+ VEX.128.66.0F ^DF ;
: VPMAXUBY+ VEX.256.66.0F ^DE ; : VPANDNY+ VEX.256.66.0F ^DF ;

:  PAVGBX<   ^66 OPL ^0F ^E0 ; :  PSRAWX<   ^66 OPL ^0F ^E1 ;
: VPAVGBX+ VEX.128.66.0F ^E0 ; : VPSRAWX+ VEX.128.66.0F ^E1 ;
: VPAVGBY+ VEX.256.66.0F ^E0 ; : VPSRAWY+ VEX.256.66.0F ^E1 ;

:  PSRADX<   ^66 OPL ^0F ^E2 ; :  PAVGWX<   ^66 OPL ^0F ^E3 ;
: VPSRADX+ VEX.128.66.0F ^E2 ; : VPAVGWX+ VEX.128.66.0F ^E3 ;
: VPSRADY+ VEX.256.66.0F ^E2 ; : VPAVGWY+ VEX.256.66.0F ^E3 ;

:  PMULHUWX<   ^66 OPL ^0F ^E4 ; :  PMULHWX<   ^66 OPL ^0F ^E5 ;
: VPMULHUWX+ VEX.128.66.0F ^E4 ; : VPMULHWX+ VEX.128.66.0F ^E5 ;
: VPMULHUWY+ VEX.256.66.0F ^E4 ; : VPMULHWY+ VEX.256.66.0F ^E5 ;

:  PCVTTPD2DQX<   ^66 OPL ^0F ^E6 ; :  MOVNTDQXM>   ^66 OPL ^0F ^E7 ;
: VPCVTTPD2DQX< VEX.128.66.0F ^E6 ; : VMOVNTDQXM> VEX.128.66.0F ^E7 ;
: VPCVTTPD2DQY< VEX.256.66.0F ^E6 ; : VMOVNTDQYM> VEX.256.66.0F ^E7 ;
:   PCVTDQ2PDX<   ^F3 OPL ^0F ^E6 ;
:  VPCVTDQ2PDX< VEX.128.F3.0F ^E6 ;
:  VPCVTDQ2PDY< VEX.256.F3.0F ^E6 ;
:   PCVTPD2DQX<   ^F2 OPL ^0F ^E6 ;
:  VPCVTPD2DQX< VEX.128.F2.0F ^E6 ;
:  VPCVTPD2DQY< VEX.256.F2.0F ^E6 ;

:  PSUBSBX<   ^66 OPL ^0F ^E8 ; :  PSUBSWX<   ^66 OPL ^0F ^E9 ;
: VPSUBSBX+ VEX.128.66.0F ^E8 ; : VPSUBSWX+ VEX.128.66.0F ^E9 ;
: VPSUBSBY+ VEX.256.66.0F ^E8 ; : VPSUBSWY+ VEX.256.66.0F ^E9 ;

:  PMINSWX<   ^66 OPL ^0F ^EA ; :  PORX<   ^66 OPL ^0F ^EB ;
: VPMINSWX+ VEX.128.66.0F ^EA ; : VPORX+ VEX.128.66.0F ^EB ;
: VPMINSWY+ VEX.256.66.0F ^EA ; : VPORY+ VEX.256.66.0F ^EB ;

:  PADDSBX<   ^66 OPL ^0F ^EC ; :  PADDSWX<   ^66 OPL ^0F ^ED ;
: VPADDSBX+ VEX.128.66.0F ^EC ; : VPADDSWX+ VEX.128.66.0F ^ED ;
: VPADDSBY+ VEX.256.66.0F ^EC ; : VPADDSWY+ VEX.256.66.0F ^ED ;

:  PMAXSWX<   ^66 OPL ^0F ^EE ; :  PXORX<   ^66 OPL ^0F ^EF ;
: VPMAXSWX+ VEX.128.66.0F ^EE ; : VPXORX+ VEX.128.66.0F ^EF ;
: VPMAXSWY+ VEX.256.66.0F ^EE ; : VPXORY+ VEX.256.66.0F ^EF ;

:  LDDQUXM<   ^F2 OPL ^0F ^F0 ; :  PSLLWX<   ^66 OPL ^0F ^F1 ;
: VLDDQUXM< VEX.128.F2.0F ^F0 ; : VPSLLWX+ VEX.128.66.0F ^F1 ;
: VLDDQUYM< VEX.256.F2.0F ^F0 ; : VPSLLWY+ VEX.256.66.0F ^F1 ;

:  PSLLDX<   ^66 OPL ^0F ^F2 ; :  PSLLQX<   ^66 OPL ^0F ^F3 ;
: VPSLLDX+ VEX.128.66.0F ^F2 ; : VPSLLQX+ VEX.128.66.0F ^F3 ;
: VPSLLDY+ VEX.256.66.0F ^F2 ; : VPSLLQY+ VEX.256.66.0F ^F3 ;

:  PMULUDQX<   ^66 OPL ^0F ^F4 ; :  PMADDWDX<   ^66 OPL ^0F ^F5 ;
: VPMULUDQX+ VEX.128.66.0F ^F4 ; : VPMADDWDX+ VEX.128.66.0F ^F5 ;
: VPMULUDQY+ VEX.256.66.0F ^F4 ; : VPMADDWDY+ VEX.256.66.0F ^F5 ;

:  PSADBWX<   ^66 OPL ^0F ^F6 ; :  MASKMOVDQU<   ^66 OPL ^0F ^F7 ;
: VPSADBWX+ VEX.128.66.0F ^F6 ; : VMASKMOVDQU< VEX.128.66.0F ^F7 ;
: VPSADBWY+ VEX.256.66.0F ^F6 ;

:  PSUBBX<   ^66 OPL ^0F ^F8 ; :  PSUBWX<   ^66 OPL ^0F ^F9 ;
: VPSUBBX+ VEX.128.66.0F ^F8 ; : VPSUBWX+ VEX.128.66.0F ^F9 ;
: VPSUBBY+ VEX.256.66.0F ^F8 ; : VPSUBWY+ VEX.256.66.0F ^F9 ;

:  PSUBDX<   ^66 OPL ^0F ^FA ; :  PSUBQX<   ^66 OPL ^0F ^FB ;
: VPSUBDX+ VEX.128.66.0F ^FA ; : VPSUBQX+ VEX.128.66.0F ^FB ;
: VPSUBDY+ VEX.256.66.0F ^FA ; : VPSUBQY+ VEX.256.66.0F ^FB ;

:  PADDBX<   ^66 OPL ^0F ^FC ; :  PADDWX<   ^66 OPL ^0F ^FD ;
: VPADDBX+ VEX.128.66.0F ^FC ; : VPADDWX+ VEX.128.66.0F ^FD ;
: VPADDBY+ VEX.256.66.0F ^FC ; : VPADDWY+ VEX.256.66.0F ^FD ;

:  PADDDX<   ^66 OPL ^0F ^FE ;
: VPADDDX+ VEX.128.66.0F ^FE ;
: VPADDDY+ VEX.256.66.0F ^FE ;

:  PSHUFBX< ^66 OPL ^0F ^38 ^00 ; :  PHADDWX< ^66 OPL ^0F ^38 ^01 ;
: VPSHUFBX+ VEX.128.66.0F38 ^00 ; : VPHADDWX+ VEX.128.66.0F38 ^01 ;
: VPSHUFBY+ VEX.256.66.0F38 ^00 ; : VPHADDWY+ VEX.256.66.0F38 ^01 ;

:  PHADDDX< ^66 OPL ^0F ^38 ^02 ; :  PHADDSWX< ^66 OPL ^0F ^38 ^03 ;
: VPHADDDX+ VEX.128.66.0F38 ^02 ; : VPHADDSWX+ VEX.128.66.0F38 ^03 ;
: VPHADDDY+ VEX.256.66.0F38 ^02 ; : VPHADDSWY+ VEX.256.66.0F38 ^03 ;

:  PMADDUBSX< ^66 OPL ^0F ^38 ^04 ; :  PHSUBWX< ^66 OPL ^0F ^38 ^05 ;
: VPMADDUBSX+ VEX.128.66.0F38 ^04 ; : VPHSUBWX+ VEX.128.66.0F38 ^05 ;
: VPMADDUBSY+ VEX.256.66.0F38 ^04 ; : VPHSUBWY+ VEX.256.66.0F38 ^05 ;

:  PHSUBDX< ^66 OPL ^0F ^38 ^06 ; :  PHSUBSWX< ^66 OPL ^0F ^38 ^07 ;
: VPHSUBDX+ VEX.128.66.0F38 ^06 ; : VPHSUBSWX+ VEX.128.66.0F38 ^07 ;
: VPHSUBDY+ VEX.256.66.0F38 ^06 ; : VPHSUBSWY+ VEX.256.66.0F38 ^07 ;

:  PSIGNBX< ^66 OPL ^0F ^38 ^08 ; :  PSIGNWX< ^66 OPL ^0F ^38 ^09 ;
: VPSIGNBX+ VEX.128.66.0F38 ^08 ; : VPSIGNWX+ VEX.128.66.0F38 ^09 ;
: VPSIGNBY+ VEX.256.66.0F38 ^08 ; : VPSIGNWY+ VEX.256.66.0F38 ^09 ;

:  PSIGNDX< ^66 OPL ^0F ^38 ^0A ; :  PMULHRSWX< ^66 OPL ^0F ^38 ^0B ;
: VPSIGNDX+ VEX.128.66.0F38 ^0A ; : VPMULHRSWX+ VEX.128.66.0F38 ^0B ;
: VPSIGNDY+ VEX.256.66.0F38 ^0A ; : VPMULHRSWY+ VEX.256.66.0F38 ^0B ;

: VPERMILPSX+ VEX.128.66.0F38 ^0C ; : VPERMILPDX+ VEX.128.66.0F38 ^0D ;
: VPERMILPSY+ VEX.256.66.0F38 ^0C ; : VPERMILPDY+ VEX.256.66.0F38 ^0D ;

: VTESTPSX< VEX.128.66.0F38 ^0E ; : VTESTPDX< VEX.128.66.0F38 ^0F ;
: VTESTPSY< VEX.256.66.0F38 ^0E ; : VTESTPDY< VEX.256.66.0F38 ^0F ;

: PBLENDVBX< ^66 OPL ^0F ^38 ^10 ; : VCVTPH2PSX< VEX.128.66.0F38 ^13 ;
                                   : VCVTPH2PSY< VEX.256.66.0F38 ^13 ;

: BLENDVPSX< ^66 OPL ^0F ^38 ^14 ; : BLENDVPDX< ^66 OPL ^0F ^38 ^15 ;

                                  :  PTESTX< ^66 OPL ^0F ^38 ^17 ;
                                  : VPTESTX< VEX.128.66.0F38 ^17 ;
: VPERMPSY+ VEX.256.66.0F38 ^16 ; : VPTESTY< VEX.256.66.0F38 ^17 ;

: VBROADCASTSSX< VEX.128.66.0F38 ^18 ;
: VBROADCASTSSY< VEX.256.66.0F38 ^18 ; : VBROADCASTSDY< VEX.256.66.0F38 ^19 ;

: VBROADCASTF128YM< VEX.256.66.0F38 ^1A ;

:  PABSBX< ^66 OPL ^0F ^38 ^1C ; :  PABSWX< ^66 OPL ^0F ^38 ^1D ;
: VPABSBX< VEX.128.66.0F38 ^1C ; : VPABSWX< VEX.128.66.0F38 ^1D ;
: VPABSBY< VEX.256.66.0F38 ^1C ; : VPABSWY< VEX.256.66.0F38 ^1D ;

:  PABSDX< ^66 OPL ^0F ^38 ^1E ;
: VPABSDX< VEX.128.66.0F38 ^1E ;
: VPABSDY< VEX.256.66.0F38 ^1E ;

[KERNEL-DEFINITIONS]

: L=0 CALL> ; \ TODO panic if L != 0
: L=1 CALL> ; \ TODO panic if L != 1
: L=2 CALL> ; \ TODO panic if L != 2

[ASSEMBLER-DEFINITIONS]

\ Encoding the ModR/M byte starting with the REG field.
\ We set MOD=3 by default, so we can reuse plain register names
\ for some operands (see above).
: RAX, ^C0    ; : RCX, ^C8    ; : RDX, ^D0    ; : RBX, ^D8    ;
: RSP, ^E0    ; : RBP, ^E8    ; : RSI, ^F0    ; : RDI, ^F8    ;
: R8,  ^C0 +R ; : R9,  ^C8 +R ; : R10, ^D0 +R ; : R11, ^D8 +R ;
: R12, ^E0 +R ; : R13, ^E8 +R ; : R14, ^F0 +R ; : R15, ^F8 +R ;

: XMM0,  ^C0    L=0 ; : YMM0,  ^C0    L=1 ; : ZMM0,  ^C0    L=2 ;
: XMM1,  ^C8    L=0 ; : YMM1,  ^C8    L=1 ; : ZMM1,  ^C8    L=2 ;
: XMM2,  ^D0    L=0 ; : YMM2,  ^D0    L=1 ; : ZMM2,  ^D0    L=2 ;
: XMM3,  ^D8    L=0 ; : YMM3,  ^D8    L=1 ; : ZMM3,  ^D8    L=2 ;
: XMM4,  ^E0    L=0 ; : YMM4,  ^E0    L=1 ; : ZMM4,  ^E0    L=2 ;
: XMM5,  ^E8    L=0 ; : YMM5,  ^E8    L=1 ; : ZMM5,  ^E8    L=2 ;
: XMM6,  ^F0    L=0 ; : YMM6,  ^F0    L=1 ; : ZMM6,  ^F0    L=2 ;
: XMM7,  ^F8    L=0 ; : YMM7,  ^F8    L=1 ; : ZMM7,  ^F8    L=2 ;
: XMM8,  ^C0 +R L=0 ; : YMM8,  ^C0 +R L=1 ; : ZMM8,  ^C0 +R L=2 ;
: XMM9,  ^C8 +R L=0 ; : YMM9,  ^C8 +R L=1 ; : ZMM9,  ^C8 +R L=2 ;
: XMM10, ^D0 +R L=0 ; : YMM10, ^D0 +R L=1 ; : ZMM10, ^D0 +R L=2 ;
: XMM11, ^D8 +R L=0 ; : YMM11, ^D8 +R L=1 ; : ZMM11, ^D8 +R L=2 ;
: XMM12, ^E0 +R L=0 ; : YMM12, ^E0 +R L=1 ; : ZMM12, ^E0 +R L=2 ;
: XMM13, ^E8 +R L=0 ; : YMM13, ^E8 +R L=1 ; : ZMM13, ^E8 +R L=2 ;
: XMM14, ^F0 +R L=0 ; : YMM14, ^F0 +R L=1 ; : ZMM14, ^F0 +R L=2 ;
: XMM15, ^F8 +R L=0 ; : YMM15, ^F8 +R L=1 ; : ZMM15, ^F8 +R L=2 ;

[KERNEL-DEFINITIONS]

: >VVVVV ( AL:0..31 R8:EVEX|VEX2|VEX3 -- R8:EVEX ) CALL>
    \ Turn VEX prefixes into EVEX prefix, and update the
    \ VVVVV field in EVEX prefix.
    \ TODO
    ;

: >VVVV ( AL:0..15 R8:EVEX|VEX2|VEX3 -- R8:EVEX|VEX2|VEX3 ) CALL>
    \ Update VEX/EVEX prefix with vvvv given in AL.
    IS-EVEX
    $75 $06             \ JNZ +6
    >VVVVV
    $C3                 \ RET
    $24 $0F             \ AND-AL. 0x0F
    $34 $0F             \ XOR-AL. 0x0F
    $C0 $E0 $03         \ SHLB_. RAX 0x03
    IS-VEX2
    $75 $0A             \ JNZ +10
    $41 $80 $60 $01 $87 \ ANDB_. [R8]. 0x01 0x87
    $41 $08 $40 $01     \ ORB> RAX, [R8]. 0x01
    $C3                 \ RET
    IS-VEX3
    $75 $0A
    $41 $80 $60 $02 $87 \ ANDB_. [R8]. 0x02 0x87
    $41 $08 $40 $02     \ ORB> RAX, [R8]. 0x02
    $C3                 \ RET
    ;                   \ TODO panic because we don't have a VEX prefix

: V=0 CALL> $50 $B0 $00 >VVVV $58 ; : V=8  CALL> $50 $B0 $08 >VVVV $58 ;
: V=1 CALL> $50 $B0 $01 >VVVV $58 ; : V=9  CALL> $50 $B0 $09 >VVVV $58 ;
: V=2 CALL> $50 $B0 $02 >VVVV $58 ; : V=10 CALL> $50 $B0 $0A >VVVV $58 ;
: V=3 CALL> $50 $B0 $03 >VVVV $58 ; : V=11 CALL> $50 $B0 $0B >VVVV $58 ;
: V=4 CALL> $50 $B0 $04 >VVVV $58 ; : V=12 CALL> $50 $B0 $0C >VVVV $58 ;
: V=5 CALL> $50 $B0 $05 >VVVV $58 ; : V=13 CALL> $50 $B0 $0D >VVVV $58 ;
: V=6 CALL> $50 $B0 $06 >VVVV $58 ; : V=14 CALL> $50 $B0 $0E >VVVV $58 ;
: V=7 CALL> $50 $B0 $07 >VVVV $58 ; : V=15 CALL> $50 $B0 $0F >VVVV $58 ;

[ASSEMBLER-DEFINITIONS]

: RAX+ V=0  ; : RCX+ V=1  ; : RDX+ V=2  ; : RBX+ V=3  ;
: RSP+ V=4  ; : RBP+ V=5  ; : RSI+ V=6  ; : RDI+ V=7  ;
: R8+  V=8  ; : R9+  V=9  ; : R10+ V=10 ; : R11+ V=11 ;
: R12+ V=12 ; : R13+ V=13 ; : R14+ V=14 ; : R14+ V=15 ;

: XMM0+  V=0  L=0 ; : YMM0+  V=0  L=1 ; : ZMM0+  V=0  L=2 ;
: XMM1+  V=1  L=0 ; : YMM1+  V=1  L=1 ; : ZMM1+  V=1  L=2 ;
: XMM2+  V=2  L=0 ; : YMM2+  V=2  L=1 ; : ZMM2+  V=2  L=2 ;
: XMM3+  V=3  L=0 ; : YMM3+  V=3  L=1 ; : ZMM3+  V=3  L=2 ;
: XMM4+  V=4  L=0 ; : YMM4+  V=4  L=1 ; : ZMM4+  V=4  L=2 ;
: XMM5+  V=5  L=0 ; : YMM5+  V=5  L=1 ; : ZMM5+  V=5  L=2 ;
: XMM6+  V=6  L=0 ; : YMM6+  V=6  L=1 ; : ZMM6+  V=6  L=2 ;
: XMM7+  V=7  L=0 ; : YMM7+  V=7  L=1 ; : ZMM7+  V=7  L=2 ;
: XMM8+  V=8  L=0 ; : YMM8+  V=8  L=1 ; : ZMM8+  V=8  L=2 ;
: XMM9+  V=9  L=0 ; : YMM9+  V=9  L=1 ; : ZMM9+  V=9  L=2 ;
: XMM10+ V=10 L=0 ; : YMM10+ V=10 L=1 ; : ZMM10+ V=10 L=2 ;
: XMM11+ V=11 L=0 ; : YMM11+ V=11 L=1 ; : ZMM11+ V=11 L=2 ;
: XMM12+ V=12 L=0 ; : YMM12+ V=12 L=1 ; : ZMM12+ V=12 L=2 ;
: XMM13+ V=13 L=0 ; : YMM13+ V=13 L=1 ; : ZMM13+ V=13 L=2 ;
: XMM14+ V=14 L=0 ; : YMM14+ V=14 L=1 ; : ZMM14+ V=14 L=2 ;
: XMM15+ V=15 L=0 ; : YMM15+ V=15 L=1 ; : ZMM15+ V=15 L=2 ;

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

: XMM0 MOD=3 RM=0 L=0 ; : XMM8  MOD=3 RM=0 L=0 +B ;
: XMM1 MOD=3 RM=1 L=0 ; : XMM9  MOD=3 RM=1 L=0 +B ;
: XMM2 MOD=3 RM=2 L=0 ; : XMM10 MOD=3 RM=2 L=0 +B ;
: XMM3 MOD=3 RM=3 L=0 ; : XMM11 MOD=3 RM=3 L=0 +B ;
: XMM4 MOD=3 RM=4 L=0 ; : XMM12 MOD=3 RM=4 L=0 +B ;
: XMM5 MOD=3 RM=5 L=0 ; : XMM13 MOD=3 RM=5 L=0 +B ;
: XMM6 MOD=3 RM=6 L=0 ; : XMM14 MOD=3 RM=6 L=0 +B ;
: XMM7 MOD=3 RM=7 L=0 ; : XMM15 MOD=3 RM=7 L=0 +B ;
: YMM0 MOD=3 RM=0 L=1 ; : YMM8  MOD=3 RM=0 L=1 +B ;
: YMM1 MOD=3 RM=1 L=1 ; : YMM9  MOD=3 RM=1 L=1 +B ;
: YMM2 MOD=3 RM=2 L=1 ; : YMM10 MOD=3 RM=2 L=1 +B ;
: YMM3 MOD=3 RM=3 L=1 ; : YMM11 MOD=3 RM=3 L=1 +B ;
: YMM4 MOD=3 RM=4 L=1 ; : YMM12 MOD=3 RM=4 L=1 +B ;
: YMM5 MOD=3 RM=5 L=1 ; : YMM13 MOD=3 RM=5 L=1 +B ;
: YMM6 MOD=3 RM=6 L=1 ; : YMM14 MOD=3 RM=6 L=1 +B ;
: YMM7 MOD=3 RM=7 L=1 ; : YMM15 MOD=3 RM=7 L=1 +B ;
: ZMM0 MOD=3 RM=0 L=2 ; : ZMM8  MOD=3 RM=0 L=2 +B ;
: ZMM1 MOD=3 RM=1 L=2 ; : ZMM9  MOD=3 RM=1 L=2 +B ;
: ZMM2 MOD=3 RM=2 L=2 ; : ZMM10 MOD=3 RM=2 L=2 +B ;
: ZMM3 MOD=3 RM=3 L=2 ; : ZMM11 MOD=3 RM=3 L=2 +B ;
: ZMM4 MOD=3 RM=4 L=2 ; : ZMM12 MOD=3 RM=4 L=2 +B ;
: ZMM5 MOD=3 RM=5 L=2 ; : ZMM13 MOD=3 RM=5 L=2 +B ;
: ZMM6 MOD=3 RM=6 L=2 ; : ZMM14 MOD=3 RM=6 L=2 +B ;
: ZMM7 MOD=3 RM=7 L=2 ; : ZMM15 MOD=3 RM=7 L=2 +B ;

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

