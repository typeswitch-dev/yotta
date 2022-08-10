: ; ^C3 ;
: SKIP-UNTIL.
    ^8A ^15 ^00000007
    ^AC
    ^3A ^C2
    ^75 ^FB
    ^EB ^01 ;
: \ SKIP-UNTIL. $0A ;
: ( SKIP-UNTIL. $29 ;

\ Welcome to yotta! Yotta is a minimalistic forth-like language
\ that has three primitives:
\
\   $XX         Compile the byte XX (in hex).
\   ^XX         Compile code that compiles the byte XX (in hex).
\   : A         Define the word A
\
\ Once a word is defined, it can be invoked by name. All definitions are
\ executed immediately (i.e. all words are "immediate"). As a result,
\ most words will compile something when executed. Words will either emit
\ machine code directly (using ^XX), or they will use "CALL>" to emit a
\ call instruction, or "INLINE>" to inline the rest of the word.
\
\ To program in yotta, we could do everything in machine code, but it would
\ be very hard to read. Our first order of business will be to build up an
\ assembler for x86-64 machine code. This will introduce a number of words
\ that mimic assembler syntax. For example,
\       MOVQ< RAX, RCX
\ will emit the appropriate (MOV RAX, RCX) instruction, and it looks like
\ the expression, but there's actually three words being executed here,
\ "MOVQ<", "RAX," and "RCX". Other than the caret ^XX and dollar $XX
\ notations, yotta does not have syntax, so the punctuation is part of the
\ word.
\
\ We're going to introduce a number of words that are only useful for
\ assembling x86-64 instructions, so we'll place them in a separate
\ ASSEMBLER wordlist. So actually our zero-th order of business is to
\ define a few words that control which wordlist we're creating new
\ words in. Unfortunately we have to write these directly in machine code.

\ User field offsets. Keep this in sync with "struc USER" in the ASM source.
\ These are memory offsets from RBP, for various fields that the engine
\ manages. RBP points to the "user area" and also to a big chunk of R/W
\ memory initially.
: $USER.HERE ^00000000 ; \ HERE pointer.
: $USER.PROG ^00000008 ; \ PROG pointer -- pointer into base of program memory.
: $USER.KERS ^00000010 ; \ Kernel source pointer.
: $USER.EVAL ^00000018 ; \ Literal evaluator pointer.
: $USER.NAME ^00000020 ; \ Name buffer.
: $USER.KERW ^00000040 ; \ Kernel wordlist.
: $USER.MINW ^00000048 ; \ Minimal wordlist. (What you get when you use ONLY.)
: $USER.ASMW ^00000050 ; \ Assembler wordlist.
: $USER.FORW ^00000058 ; \ Forth wordlist.
: $USER.MIRW ^00000060 ; \ Mirth wordlist.
: $USER.DICT ^00000068 ; \ Dictionary pointer.
: $USER.FIND ^00000070 ;

\ Dict field offsets. Keep in sync with "struc DICT" in the ASM source.
\ The dictionary is a linked list of definitions.
: $DICT.LINK ^00 ; \ Link to previous dictionary entry.
: $DICT.NAME ^08 ; \ Length byte + 31 bytes of name data, padded with spaces.
: $DICT.CODE ^28 ; \ Pointer to code that will get executed when word is read.
: $DICT.DATA ^30 ; \ Extra data associated with the word.

: [KERNEL-DEFINITIONS]
    \ Change current dictionary to KERNEL.
    $48 $8D $85 $USER.KERW \ LEA RAX, [RBP+USER.KERW]
    $48 $89 $85 $USER.DICT \ MOV [RBP+USER.DICT], RAX
    ;

: [MIRTH-DEFINITIONS]
    \ Change current dictionary to MIRTH.
    $48 $8D $85 $USER.MIRW \ LEA RAX, [RBP+USER.MIRW]
    $48 $89 $85 $USER.DICT \ MOV [RBP+USER.DICT], RAX
    ;

: [FORTH-DEFINITIONS]
    \ Change current dictionary to FORTH.
    $48 $8D $85 $USER.FORW \ LEA RAX, [RBP+USER.FORW]
    $48 $89 $85 $USER.DICT \ MOV [RBP+USER.DICT], RAX
    ;

: [ASSEMBLER-DEFINITIONS]
    \ Change current dictionary to ASSEMBLER.
    $48 $8D $85 $USER.ASMW \ LEA RAX, [RBP+USER.ASMW]
    $48 $89 $85 $USER.DICT \ MOV [RBP+USER.DICT], RAX
    ;

: [MINIMAL-DEFINITIONS]
    \ Change current dictionary to ASSEMBLER.
    $48 $8D $85 $USER.MINW \ LEA RAX, [RBP+USER.MINW]
    $48 $89 $85 $USER.DICT \ MOV [RBP+USER.DICT], RAX
    ;

: PREPARE-REX
    \ "Header" of PREPARE-REX, which compiles a call to the body.
    \ This is what gets executed when PREPARE-REX is interpreted.
    \ Note that this is the same as the header of CALL> ... it's
    \ just in machine code because we can't implement CALL>
    \ nicely yet.
    ^E8                     \ emit E8 byte ( E8 xx xx xx xx = CALL NEAR )
    $48 $8D $05 $00000001   \ LEA RAX, [RIP+1]
    $48 $2B $C7             \ SUB RAX, RDI
    $AB                     \ STOSL
    $C3                     \ RET

    \ "Body" of PREPARE-REX. This gets executed when the
    \ word that called PREPARE-REX is run, so the instructions
    \ below are emitted alongside any other instructions emitted
    \ by PREPARE-REX's caller. I.e. so we know that REX is prepared
    \ before any instruction that follows.
    ^4B ^85 ^C0        \ TEST R8, R8
    ^75 ^12            \ JNZ +18
    ^4B ^8B ^01        \ MOV R8, [R9]
    ^4B ^89 ^41 ^01    \ MOV [R9+1], R8
    ^4B ^8B ^C1        \ MOV R8, R9
    ^41 ^C6 ^00 ^40    \ MOV [R8], 0x40
    ^4B ^8D ^49 ^01    \ LEA R9, [R9+1]
    ;

: +W PREPARE-REX ^49 ^80 ^08 ^08 ; \ OR [R8], 0x08
: +R PREPARE-REX ^49 ^80 ^08 ^04 ; \ OR [R8], 0x04
: +X PREPARE-REX ^49 ^80 ^08 ^02 ; \ OR [R8], 0x02
: +B PREPARE-REX ^49 ^80 ^08 ^01 ; \ OR [R8], 0x01

: OPB ^49 ^89 ^F9 ; \ MOV R9, RDI

: OPW ^B0 ^66 ^AA   \ MOV AL, 0x66; STOSB
      ^45 ^33 ^C0 ; \ XOR R8, R8
      ^49 ^89 ^F9 ; \ MOV R9, RDI

: OPL ^45 ^33 ^C0 ; \ XOR R8, R8
      ^49 ^89 ^F9 ; \ MOV R9, RDI

: OPQ ^49 ^89 ^F8   \ MOV R8, RDI
      ^B0 ^48 ^AA   \ MOV AL, 0x48; STOSB
      ^49 ^89 ^F9 ; \ MOV R9, RDI

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
: MOVSB OPB ^A4 ; : MOVSW OPW ^A5 ; : MOVSL OPL ^A5 ; : MOVSQ OPQ ^A5 ;
: CMPSB OPB ^A6 ; : CMPSW OPW ^A7 ; : CMPSL OPL ^A7 ; : CMPSQ OPQ ^A7 ;
: STOSB OPB ^AA ; : STOSW OPW ^AB ; : STOSL OPL ^AB ; : STOSQ OPQ ^AB ;
: LODSB OPB ^AC ; : LODSW OPW ^AD ; : LODSL OPL ^AD ; : LODSQ OPQ ^AD ;
: SCASB OPB ^AE ; : SCASW OPW ^AF ; : SCASL OPL ^AF ; : LODSQ OPQ ^AF ;
: IMULB OPB ^F6 ; : IMULW OPW ^F7 ; : IMULL OPL ^F7 ; : IMULQ OPQ ^F7 ;

: CWD OPW ^99 ; : CDQ OPL ^99 ; : CQO OPQ ^99 ;

: RET OPL ^C3 ;
: SYSCALL ^0F ^05 ;

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

: MOVB> OPB ^88 ; : MOVB< OPB ^8A ;
: MOVW> OPW ^89 ; : MOVW< OPW ^8B ;
: MOVL> OPL ^89 ; : MOVL< OPL ^8B ;
: MOVQ> OPQ ^89 ; : MOVQ< OPQ ^8B ;

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
: PUSHQ.  OPB ^6A ; \ Pushes 64-bits even without REX.W prefix. TODO verify
: PUSHQ:  OPL ^68 ; \ Pushes 64-bits even without REX.W prefix. TODO verify

\ Ops that expect a direct register argument.
\ (Do not write a comma after the direct register argument!)
\ E.g. PUSHQ# RAX
\      POPQ# R15
: PUSHW# OPW ^50 ; : PUSHQ# OPL ^50 ; \ forced to 64 bits
: POPW#  OPW ^58 ; : POPQ#  OPL ^58 ; \ forced to 64 bits

\ Ops that expect a direct register argument and an immediate.
\ (Do not write a comma after the direct register argument!)
\ E.g.  MOVL#: RAX $10203040  ( MOV EAX, 10203040h )
: MOVB#.  OPB ^B0 ;
: MOVW#.. OPW ^B8 ;
: MOVL#:  OPL ^B8 ;
: MOVQ#:: OPQ ^B8 ;

\ Ops that expect a modrm argument only.

:  POPW_ OPW ^8F /0 ; :  POPQ_ OPL ^8F /0 ; \ forced to 64 bits
: PUSHW_ OPW ^FF /6 ; : PUSHQ_ OPL ^FF /6 ; \ forced to 64 bits

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

[KERNEL-DEFINITIONS]

: CALL> ( -- )
    \ Exit the current word, and compile a call to the rest
    \ of the word. The typical usage is at the start of a word
    \ definition, to turn the whole definition into a word that
    \ simply compiles a call to itself. For example,
    \
    \   : SQUARE CALL> DUP * ;
    \
    \ will make it so that when SQUARE is interpreted, it will
    \ compile a call into its body (the DUP * part). Thus CALL>
    \ is like the default compilation action of most forths.

    ^E8
    LEAQ< RAX, [RIP]: $00000001
    SUBQ< RAX, RDI
    STOSL
    RET

    ^E8
    POPQ# RAX
    LEAQ< RAX, [RAX]. $FC
    SUBQ< RAX, RDI
    STOSL ;

: ;
    \ Store the end address of the word in
    \ its dictionary entry, and emit RET.
    MOVQ< RAX, [RBP]: $USER.DICT
    MOVQ< RAX, [RAX]
    MOVQ> RDI, [RAX]. $DICT.DATA
    ^C3 \ RET
    ;

: INLINE> ( -- ) CALL>
    \ Changes the action of a word to be the inlining of
    \ its contents. Only do this if the word contains
    \ no referneces (e.g. CALLs) because otherwise the
    \ reference will be all wrong.
    MOVQ< RDX, RSI
    POPQ# RSI
    MOVQ< RCX, [RAX]. $DICT.DATA
    SUBQ< RCX, RSI
    REP MOVSB
    MOVQ< RSI, RDX
    ;

: INLINE/CALL> ( -- ) CALL>
    \ Same as INLINE> but indicates that it would
    \ be ok to call this word directly too.
    MOVQ< RDX, RSI
    POPQ# RSI
    MOVQ< RCX, [RAX]. $DICT.DATA
    SUBQ< RCX, RSI
    REP MOVSB
    MOVQ< RSI, RDX
    ;

: [SETUP]
    \ Set up data stack (R15).
    LEAQ< R15, [RBP]: $10000000
    ;
[SETUP]

[FORTH-DEFINITIONS]

\ Basic stack words. These preserve non-stack registers (RBX, R15).
: DUP ( a -- a a ) INLINE/CALL>
    LEAQ< R15, [R15]. $F8
    MOVQ> RBX, [R15] ;
: NIP ( a b -- b ) INLINE/CALL>
    LEAQ< R15, [R15]. $08 ;
: DROP ( a -- ) INLINE/CALL>
    MOVQ< RBX, [R15]
    LEAQ< R15, [R15]. $08 ;

\ Deeper stack words. These can trash RAX and RDX.
: SWAP ( a b -- b a ) INLINE/CALL>
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]
    MOVQ< RBX, RAX ;
: rotl ( a b c -- b c a ) INLINE/CALL>
    MOVQ< RAX, [R15]. $08
    MOVQ< RDX, [R15]
    MOVQ> RBX, [R15]
    MOVQ> RBX, [R15]. $08
    MOVQ< RBX, RAX ;
: rotr ( a b c -- c a b ) INLINE/CALL>
    MOVQ< RDX, [R15]. $08
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]. $08
    MOVQ> RDX, [R15]
    MOVQ< RBX, RAX ;

: RDROP ( R: x -- ) INLINE>
    \ Drop top of return stack.
    LEAQ< RSP, [0+ RSP]. $08 ;

: 0* ( x -- 0 ) INLINE/CALL>
    XORL< RBX, RBX ;
: 0 ( -- 0 ) INLINE/CALL>
    \ Push zero on data stack.
    DUP 0* ;

[ASSEMBLER-DEFINITIONS]

: U. ( -- u ) INLINE>
    \ Push zero-extended byte literal on data stack.
    DUP
    XORL< RBX, RBX
    MOVB#. RBX ;
: U: ( -- u ) INLINE>
    \ Push zero-extended 32-bit literal on data stack.
    DUP MOVL#: RBX ;
: I: ( -- n ) INLINE>
    \ Push sign-extended 32-bit literal on data stack.
    DUP MOVQ_: RBX ;
: X:: ( -- x ) INLINE>
    \ Push 64-bit literal on data stack.
    DUP MOVQ#:: RBX ;

[KERNEL-DEFINITIONS]

\ syscall numbers (darwin) as 32-bit immediates
: $SYS_EXIT  ^02000001 ;
: $SYS_FORK  ^02000002 ;
: $SYS_READ  ^02000003 ;
: $SYS_WRITE ^02000004 ;
: $SYS_OPEN  ^02000005 ;
: $SYS_CLOSE ^02000006 ;
: $SYS_MMAP  ^020000C5 ;

[FORTH-DEFINITIONS]

: BYE ( -- ) CALL>
    \ End the program.
    MOVL#: RAX $SYS_EXIT
    XORL< RDI, RDI
    SYSCALL ;

: EMIT ( c -- ) CALL>
    \ Emit byte to stdout.
    PUSHQ# RDI
    PUSHQ# RSI
    PUSHQ# RBX
    MOVL#: RAX $SYS_WRITE
    MOVL#: RDI $00000001 \ stdout
    MOVL#: RDX $00000001 \ length
    MOVQ< RSI, RSP \ source
    SYSCALL
    RDROP
    POPQ# RSI
    POPQ# RDI
    DROP ;

: 'NUL' INLINE/CALL>     0  ; : 'BL'  INLINE/CALL> U. $20 ;
: 'SOH' INLINE/CALL> U. $01 ; : '!'   INLINE/CALL> U. $21 ;
: 'STX' INLINE/CALL> U. $02 ; : '"'   INLINE/CALL> U. $22 ;
: 'ETX' INLINE/CALL> U. $03 ; : '#'   INLINE/CALL> U. $23 ;
: 'SOT' INLINE/CALL> U. $04 ; : '$'   INLINE/CALL> U. $24 ;
: 'ENQ' INLINE/CALL> U. $05 ; : '%'   INLINE/CALL> U. $25 ;
: 'ACK' INLINE/CALL> U. $06 ; : '&'   INLINE/CALL> U. $26 ;
: 'BEL' INLINE/CALL> U. $07 ; : '''   INLINE/CALL> U. $27 ;
: 'BS'  INLINE/CALL> U. $08 ; : '('   INLINE/CALL> U. $28 ;
: 'HT'  INLINE/CALL> U. $09 ; : ')'   INLINE/CALL> U. $29 ;
: 'LF'  INLINE/CALL> U. $0A ; : '*'   INLINE/CALL> U. $2A ;
: 'VT'  INLINE/CALL> U. $0B ; : '+'   INLINE/CALL> U. $2B ;
: 'FF'  INLINE/CALL> U. $0C ; : ','   INLINE/CALL> U. $2C ;
: 'CR'  INLINE/CALL> U. $0D ; : '-'   INLINE/CALL> U. $2D ;
: 'SO'  INLINE/CALL> U. $0E ; : '.'   INLINE/CALL> U. $2E ;
: 'SI'  INLINE/CALL> U. $0F ; : '/'   INLINE/CALL> U. $2F ;
: 'DLE' INLINE/CALL> U. $10 ; : '0'   INLINE/CALL> U. $30 ;
: 'DC1' INLINE/CALL> U. $11 ; : '1'   INLINE/CALL> U. $31 ;
: 'DC2' INLINE/CALL> U. $12 ; : '2'   INLINE/CALL> U. $32 ;
: 'DC3' INLINE/CALL> U. $13 ; : '3'   INLINE/CALL> U. $33 ;
: 'DC4' INLINE/CALL> U. $14 ; : '4'   INLINE/CALL> U. $34 ;
: 'NAK' INLINE/CALL> U. $15 ; : '5'   INLINE/CALL> U. $35 ;
: 'SYN' INLINE/CALL> U. $16 ; : '6'   INLINE/CALL> U. $36 ;
: 'ETB' INLINE/CALL> U. $17 ; : '7'   INLINE/CALL> U. $37 ;
: 'CAN' INLINE/CALL> U. $18 ; : '8'   INLINE/CALL> U. $38 ;
: 'EM'  INLINE/CALL> U. $19 ; : '9'   INLINE/CALL> U. $39 ;
: 'SUB' INLINE/CALL> U. $1A ; : ':'   INLINE/CALL> U. $3A ;
: 'ESC' INLINE/CALL> U. $1B ; : ';'   INLINE/CALL> U. $3B ;
: 'FS'  INLINE/CALL> U. $1C ; : '<'   INLINE/CALL> U. $3C ;
: 'GS'  INLINE/CALL> U. $1D ; : '='   INLINE/CALL> U. $3D ;
: 'RS'  INLINE/CALL> U. $1E ; : '>'   INLINE/CALL> U. $3E ;
: 'US'  INLINE/CALL> U. $1F ; : '?'   INLINE/CALL> U. $3F ;

: '@'   INLINE/CALL> U. $40 ; : '`'   INLINE/CALL> U. $60 ;
: 'A'   INLINE/CALL> U. $41 ; : 'a'   INLINE/CALL> U. $61 ;
: 'B'   INLINE/CALL> U. $42 ; : 'b'   INLINE/CALL> U. $62 ;
: 'C'   INLINE/CALL> U. $43 ; : 'c'   INLINE/CALL> U. $63 ;
: 'D'   INLINE/CALL> U. $44 ; : 'd'   INLINE/CALL> U. $64 ;
: 'E'   INLINE/CALL> U. $45 ; : 'e'   INLINE/CALL> U. $65 ;
: 'F'   INLINE/CALL> U. $46 ; : 'f'   INLINE/CALL> U. $66 ;
: 'G'   INLINE/CALL> U. $47 ; : 'g'   INLINE/CALL> U. $67 ;
: 'H'   INLINE/CALL> U. $48 ; : 'h'   INLINE/CALL> U. $68 ;
: 'I'   INLINE/CALL> U. $49 ; : 'i'   INLINE/CALL> U. $69 ;
: 'J'   INLINE/CALL> U. $4A ; : 'j'   INLINE/CALL> U. $6A ;
: 'K'   INLINE/CALL> U. $4B ; : 'k'   INLINE/CALL> U. $6B ;
: 'L'   INLINE/CALL> U. $4C ; : 'l'   INLINE/CALL> U. $6C ;
: 'M'   INLINE/CALL> U. $4D ; : 'm'   INLINE/CALL> U. $6D ;
: 'N'   INLINE/CALL> U. $4E ; : 'n'   INLINE/CALL> U. $6E ;
: 'O'   INLINE/CALL> U. $4F ; : 'o'   INLINE/CALL> U. $6F ;
: 'P'   INLINE/CALL> U. $50 ; : 'p'   INLINE/CALL> U. $70 ;
: 'Q'   INLINE/CALL> U. $51 ; : 'q'   INLINE/CALL> U. $71 ;
: 'R'   INLINE/CALL> U. $52 ; : 'r'   INLINE/CALL> U. $72 ;
: 'S'   INLINE/CALL> U. $53 ; : 's'   INLINE/CALL> U. $73 ;
: 'T'   INLINE/CALL> U. $54 ; : 't'   INLINE/CALL> U. $74 ;
: 'U'   INLINE/CALL> U. $55 ; : 'u'   INLINE/CALL> U. $75 ;
: 'V'   INLINE/CALL> U. $56 ; : 'v'   INLINE/CALL> U. $76 ;
: 'W'   INLINE/CALL> U. $57 ; : 'w'   INLINE/CALL> U. $77 ;
: 'X'   INLINE/CALL> U. $58 ; : 'x'   INLINE/CALL> U. $78 ;
: 'Y'   INLINE/CALL> U. $59 ; : 'y'   INLINE/CALL> U. $79 ;
: 'Z'   INLINE/CALL> U. $5A ; : 'z'   INLINE/CALL> U. $7A ;
: '['   INLINE/CALL> U. $5B ; : '{'   INLINE/CALL> U. $7B ;
: '\'   INLINE/CALL> U. $5C ; : '|'   INLINE/CALL> U. $7C ;
: ']'   INLINE/CALL> U. $5D ; : '}'   INLINE/CALL> U. $7D ;
: '^'   INLINE/CALL> U. $5E ; : '~'   INLINE/CALL> U. $7E ;
: '_'   INLINE/CALL> U. $5F ; : 'DEL' INLINE/CALL> U. $7F ;

: CR ( -- ) CALL>
    \ Emit a newline to stdout.
    'LF' EMIT ;
: SPACE ( -- ) CALL>
    \ Emit a single space character to stdout.
    'BL' EMIT ;

[KERNEL-DEFINITIONS]

: $TEMPOFFSET ^0FFFC000 ;
: RDI:TEMP ( -- RDI=TEMP ) CALL>
    \ Load temporary program memory address into RDI.
    MOVQ< RDI, [RBP]: $USER.PROG
    LEAQ< RDI, [RDI]: $TEMPOFFSET ;

[FORTH-DEFINITIONS]

: [
    \ Start compiling a temporary program. The program should
    \ be finished up by ], which will then execute the program.
    \ Use this to execute stuff immediately, instead of compiling.
    \ Usage example: [ LF ]
    \ Will output a newline while compiling the program, because
    \ it compiles the LF call in a temporary location, and then
    \ runs the code that was compiled in that location.

    \ Running this while you are already compiling a temporary
    \ program will result in very strange behavior. Don't do it!
    DUP
    MOVQ< RBX, RDI
    RDI:TEMP ;

: ]
    \ Finish the temporary program, restore RDI to its original
    \ location, then run the temporary program.
    ^C3
    RDI:TEMP
    PUSHQ# RDI
    MOVQ<  RDI, RBX
    DROP
    ;

[KERNEL-DEFINITIONS]

: BRANCH-TOO-BIG CALL>
    \ TODO display message.
    MOVL#: RAX $SYS_EXIT
    MOVL#: RDI $00000010
    SYSCALL ;
: VERIFY-BRANCH-OFFSET CALL>
    CMP-RAX: $7FFFFFFF
    JLE.  $05
    BRANCH-TOO-BIG
    CMP-RAX: $80000000
    JGE.  $05
    BRANCH-TOO-BIG ;

[ASSEMBLER-DEFINITIONS]

: BRANCH> ( -- orig ) CALL>
    \ Create blank offset for forward branch.
    \ Must be resolved by >TARGET.
    DUP
    MOVQ< RBX, RDI
    XORQ< RAX, RAX
    STOSL ;
: >TARGET ( orig -- ) CALL>
    \ Resolve forward branch.
    LEAQ< RAX, [RDI]. $FC
    SUBQ< RAX, RBX
    VERIFY-BRANCH-OFFSET
    MOVL> RAX, [RBX] \ 32 bit move
    DROP ;

: TARGET< ( -- dest ) CALL>
    \ Push current location as backward branch target.
    \ May be used by <BRANCH any number of times.
    DUP
    MOVQ< RBX, RDI ;
: <BRANCH ( dest -- ) CALL>
    \ Push offset to backward branch target.
    SUBQ< RBX, RDI
    LEAQ< RAX, [RBX]. $FC
    VERIFY-BRANCH-OFFSET
    STOSL
    DROP ;

[FORTH-DEFINITIONS]

: AHEAD ( CT: -- orig ) ( RT: *a -- *a / *b )
    \ Always branches forward.
    ^E9 BRANCH> ;
: ?IF ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Non-destructive IF. Takes the first branch
    \ if top of stack is nonzero, otherwise takes
    \ the second branch.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^84 BRANCH> ; \ JZ branch
: <IF ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Non-destructive IF. Takes the first branch
    \ if top of stack is negative, otherwise takes
    \ the second branch.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^89 BRANCH> ; \ JNS branch

[ASSEMBLER-DEFINITIONS]

\ Like ?IF but use CPU flags instead of testing stack top.
: IFNO ^0F ^80 BRANCH> ; \ if no overflow      (OF=0)
: IFO  ^0F ^81 BRANCH> ; \ if overflow         (OF=1)
: IFAE ^0F ^82 BRANCH> ; \ if above or equal   (CF=0)
: IFB  ^0F ^83 BRANCH> ; \ if below            (CF=1)
: IFNZ ^0F ^84 BRANCH> ; \ if non-equal        (ZF=1)
: IFZ  ^0F ^85 BRANCH> ; \ if zero             (ZF=0)
: IFA  ^0F ^86 BRANCH> ; \ if above            (CF=0 and ZF=0)
: IFBE ^0F ^87 BRANCH> ; \ if below or equal   (CF=1 or  ZF=1)
: IFNS ^0F ^88 BRANCH> ; \ if positive         (SF=0)
: IFS  ^0F ^89 BRANCH> ; \ if negative         (SF=1)
: IFPO ^0F ^8A BRANCH> ; \ if parity odd       (PF=0)
: IFPE ^0F ^8B BRANCH> ; \ if parity even      (PF=1)
: IFGE ^0F ^8C BRANCH> ; \ if greater or equal (SF=OF)
: IFL  ^0F ^8D BRANCH> ; \ if less             (SF<>OF)
: IFG  ^0F ^8E BRANCH> ; \ if greater          (SF=OF)
: IFLE ^0F ^8F BRANCH> ; \ if less or equal    (SF<>OF)

[FORTH-DEFINITIONS]

: THEN ( CT: orig -- ) ( RT: *a / *a -- *a )
    \ Resolve a forward branch.
    >TARGET ;
: ELSE ( CT: orig1 -- orig2 ) ( RT: *a / *b -- *b / *a )
    \ Swap between branches.
    ^E9 BRANCH> SWAP >TARGET ;

: BEGIN ( CT: -- dest ) ( RT: *a -- ~*a / *a )
    \ Begin a loop.
    TARGET< ;
: AGAIN ( CT: dest -- ) ( RT: *~a / *a -- *b )
    \ Loop forever.
    \ Used at end of loop, e.g. BEGIN ... AGAIN
    ^E9 <BRANCH ;
: ?UNTIL ( CT: dest -- ) ( RT: *~a x / *a x -- *a x )
    \ Keep going until nonzero. Nondestructive.
    \ Used at end of loop, e.g. BEGIN ... ?UNTIL
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^84 <BRANCH ; \ JZ branch

[ASSEMBLER-DEFINITIONS]

\ Like ?UNTIL but using CPU flags instead of testing stack top.
: UNTILNO ^0F ^80 <BRANCH ; \ until no overflow      (OF=0)
: UNTILO  ^0F ^81 <BRANCH ; \ until overflow         (OF=1)
: UNTILAE ^0F ^82 <BRANCH ; \ until above or equal   (CF=0)
: UNTILB  ^0F ^83 <BRANCH ; \ until below            (CF=1)
: UNTILNZ ^0F ^84 <BRANCH ; \ until non-equal        (ZF=1)
: UNTILZ  ^0F ^85 <BRANCH ; \ until zero             (ZF=0)
: UNTILA  ^0F ^86 <BRANCH ; \ until above            (CF=0 and ZF=0)
: UNTILBE ^0F ^87 <BRANCH ; \ until below or equal   (CF=1 or  ZF=1)
: UNTILNS ^0F ^88 <BRANCH ; \ until positive         (SF=0)
: UNTILS  ^0F ^89 <BRANCH ; \ until negative         (SF=1)
: UNTILPO ^0F ^8A <BRANCH ; \ until parity odd       (PF=0)
: UNTILPE ^0F ^8B <BRANCH ; \ until parity even      (PF=1)
: UNTILGE ^0F ^8C <BRANCH ; \ until greater or equal (SF=OF)
: UNTILL  ^0F ^8D <BRANCH ; \ until less             (SF<>OF)
: UNTILG  ^0F ^8E <BRANCH ; \ until greater          (SF=OF)
: UNTILLE ^0F ^8F <BRANCH ; \ until less or equal    (SF<>OF)

[FORTH-DEFINITIONS]

: ?WHILE ( CT: dest -- orig dest ) ( RT: ~*a / *b x -- *b x / ~*a / *b x )
    \ Keep going while value is nonzero. Nondestructive.
    \ Typical usage: begin ... ?while ... repeat
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^84 BRANCH> ; \ JNZ branch
    SWAP ;

[ASSEMBLER-DEFINITIONS]

\ Like ?WHILE but uses CPU flags instead of testing stack top.
: WHILENO ^0F ^80 BRANCH> SWAP ; \ while no overflow      (OF=0)
: WHILEO  ^0F ^81 BRANCH> SWAP ; \ while overflow         (OF=1)
: WHILEAE ^0F ^82 BRANCH> SWAP ; \ while above or equal   (CF=0)
: WHILEB  ^0F ^83 BRANCH> SWAP ; \ while below            (CF=1)
: WHILENZ ^0F ^84 BRANCH> SWAP ; \ while non-equal        (ZF=1)
: WHILEZ  ^0F ^85 BRANCH> SWAP ; \ while zero             (ZF=0)
: WHILEA  ^0F ^86 BRANCH> SWAP ; \ while above            (CF=0 and ZF=0)
: WHILEBE ^0F ^87 BRANCH> SWAP ; \ while below or equal   (CF=1 or  ZF=1)
: WHILENS ^0F ^88 BRANCH> SWAP ; \ while positive         (SF=0)
: WHILES  ^0F ^89 BRANCH> SWAP ; \ while negative         (SF=1)
: WHILEPO ^0F ^8A BRANCH> SWAP ; \ while parity odd       (PF=0)
: WHILEPE ^0F ^8B BRANCH> SWAP ; \ while parity even      (PF=1)
: WHILEGE ^0F ^8C BRANCH> SWAP ; \ while greater or equal (SF=OF)
: WHILEL  ^0F ^8D BRANCH> SWAP ; \ while less             (SF<>OF)
: WHILEG  ^0F ^8E BRANCH> SWAP ; \ while greater          (SF=OF)
: WHILELE ^0F ^8F BRANCH> SWAP ; \ while less or equal    (SF<>OF)

[FORTH-DEFINITIONS]

: REPEAT ( CT: orig dest -- ) ( RT: *b / *~a / *a -- *b )
    \ End a "begin ... while ..." loop.
    \ This is equivalent to "again then".
    ^E9 <BRANCH >TARGET ;

[FORTH-DEFINITIONS]

: + ( n1 n2 -- n3 ) INLINE/CALL>
    \ Addition.
    ADDQ< RBX, [R15]
    NIP ;
: - ( n1 n2 -- n3 ) INLINE/CALL>
    \ Subtraction.
    MOVQ< RAX, [R15]
    NIP
    SUBQ< RAX, RBX
    MOVQ< RBX, RAX ;
: * ( n1 n2 -- n3 ) INLINE/CALL>
    \ Multiplication.
    IMULQ< RBX, [R15]
    NIP ;

: /MOD ( i1 +i2 -- i3 i4 ) INLINE/CALL>
    \ Signed integer division.
    MOVQ< RAX, [R15]
    CQO
    IDIVQ_ RBX
    MOVQ> RAX, [R15]
    MOVQ< RBX, RDX ;

: U/MOD ( u1 +u2 -- u3 u4 ) INLINE/CALL>
    \ Unsigned integer division.
    MOVQ< RAX, [R15]
    XORL< RDX, RDX
    DIVQ_ RBX
    MOVQ> RAX, [R15]
    MOVQ< RBX, RDX ;

: NEGATE ( i1 -- i2 ) INLINE/CALL>
    NEGQ_ RBX ;

: .DIGITS ( u -- ) CALL>
    \ Print decimal digits of unsigned number.
    U. $0A U/MOD SWAP
    ?IF .DIGITS ELSE DROP THEN
    '0' + EMIT ;

: . ( i -- ) CALL>
    \ Print decimal number, followed by space.
    <IF
        '-' EMIT
        NEGATE
    THEN
    .DIGITS
    SPACE ;

: PARSE-NAME ( "<spaces>name" -- c-addr u ) CALL>
    \ Parse a name from source stream.
    \ TODO Proper bounds checking.
    BEGIN
        LODSB
        CMP-AL. $20
    UNTILA
    LEAQ< R15, [R15]. $F0
    MOVQ> RBX, [R15]. $08
    LEAQ< RBX, [RSI]. $FF
    MOVQ> RBX, [R15]
    XORL< RBX, RBX
    BEGIN
        INCQ_ RBX
        LODSB
        CMP-AL. $20
    UNTILBE ;

:  I8_MAX INLINE/CALL> U.  $7F ;
:  I8_MIN INLINE/CALL> I:  $FFFFFF80 ;
:  U8_MAX INLINE/CALL> U.  $FF ;
:  U8_MIN INLINE/CALL> 0 ;
: I16_MAX INLINE/CALL> U:  $00007FFF ;
: I16_MIN INLINE/CALL> I:  $FFFF8000 ;
: U16_MAX INLINE/CALL> U:  $0000FFFF ;
: U16_MIN INLINE/CALL> 0 ;
: I32_MAX INLINE/CALL> U:  $7FFFFFFF ;
: I32_MIN INLINE/CALL> I:  $80000000 ;
: U32_MAX INLINE/CALL> U:  $FFFFFFFF ;
: U32_MIN INLINE/CALL> 0 ;
: I64_MAX INLINE/CALL> X:: $7FFFFFFFFFFFFFFF ;
: I64_MIN INLINE/CALL> X:: $8000000000000000 ;
: U64_MAX INLINE/CALL> I:  $FFFFFFFF ;
: U64_MIN INLINE/CALL> 0 ;

: MAIN CALL>
    'H' EMIT 'e' EMIT 'l' EMIT 'l' EMIT 'o' EMIT '!' EMIT CR
    U. $0A . CR
    I8_MAX . CR
    I8_MIN . CR
    I16_MAX . CR
    I16_MIN . CR
    I32_MAX . CR
    I32_MIN . CR
    I64_MAX . CR
    I64_MIN . CR
    ;

[ MAIN BYE ]

