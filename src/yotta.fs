: ; ^C3 ;
: SKIP-UNTIL.
    ^8A ^15 ^00000007
    ^AC
    ^3A ^C2
    ^75 ^FB
    ^EB ^01 ;
: \ SKIP-UNTIL. $0A ;
: ( SKIP-UNTIL. $29 ;

\ Welcome to nanoforth -- name not final! This is a minimalistic
\ forth that has three primitives:
\
\   $XX     Compile the byte XX (in hex).
\   ^XX     Compile code that compiles the byte XX (in hex).
\   : A     Define a new word A
\
\ Once a word is defined, it can be invoked using its name.
\ Unlike regular forth, there is no separate compilation &
\ interpretation phases -- all words are "immediate", i.e.
\ immediately executed.
\
\ There is a separate region of memory for non-executable data.
\ This is where the dictionary is stored, as a linked list. The
\ dictionary list is very simple:
\
\   0      8      40     48     64
\   |------|------|------|------|
\   | LINK | NAME | CODE | DATA |
\   |------|------|------|------|
\
\ The fields are as follows:
\
\   - LINK points to the previous item in the dictionary. If zero,
\     Then this is the last item in the dictionary.
\
\   - NAME contains the name of this word. The first byte is the length
\     of the name, and the subsequent 31 bytes are the name's contents,
\     which are padded out with spaces. Before storing or searching a
\     NAME, we convert the lowercase ASCII characters into uppercase.
\
\   - CODE is the address of the word that compiles this.
\
\   - DATA is padding that can be used by the kernel or program
\     to store additional information. We only reserve two qwords
\     here automatically, but more can be used.
\
\ When a word is executed by the interpreter, it will have RAX
\ as the address of the dictionary entry, so the word can easily obtain
\ the other fields if needed.

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
: POPW#  OPW ^50 ; : POPQ#  OPL ^58 ; \ forced to 64 bits

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

\ Direct register addressing.
: RAX RM=0 ; : R8  RM=0 +B ;
: RCX RM=1 ; : R9  RM=1 +B ;
: RDX RM=2 ; : R10 RM=2 +B ;
: RBX RM=3 ; : R11 RM=3 +B ;
: RSP RM=4 ; : R12 RM=4 +B ;
: RBP RM=5 ; : R13 RM=5 +B ;
: RSI RM=6 ; : R14 RM=6 +B ;
: RDI RM=7 ; : R15 RM=7 +B ;

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

\ set register to immediate value
:   AL:     ^B0 ; :   CL:     ^B1 ; :   DL:     ^B2 ; :   BL:     ^B3 ;
:   AH:     ^B4 ; :   CH:     ^B5 ; :   DH:     ^B6 ; :   BH:     ^B7 ;
:  SPL: ^40 ^B4 ; :  BPL: ^40 ^B5 ; :  SIL: ^40 ^B6 ; :  DIL: ^40 ^B7 ;
:  R8B: ^41 ^B0 ; :  R9B: ^41 ^B1 ; : R10B: ^41 ^B2 ; : R11B: ^41 ^B3 ;
: R12B: ^41 ^B4 ; : R13B: ^41 ^B5 ; : R14B: ^41 ^B6 ; : R15B: ^41 ^B7 ;

:   AX: ^66     ^B8 ; :   CX: ^66     ^B9 ; :   DX: ^66     ^BA ;
:   BX: ^66     ^BB ; :   SP: ^66     ^BC ; :   BP: ^66     ^BD ;
:   SI: ^66     ^BE ; :   DI: ^66     ^BF ;
:  R8W: ^66 ^41 ^B8 ; :  R9W: ^66 ^41 ^B9 ; : R10W: ^66 ^41 ^BA ;
: R11W: ^66 ^41 ^BB ; : R12W: ^66 ^41 ^BC ; : R13W: ^66 ^41 ^BD ;
: R14W: ^66 ^41 ^BE ; : R15W: ^66 ^41 ^BF ;

:  EAX:     ^B8 ; :  ECX:     ^B9 ; :  EDX:     ^BA ; :  EBX:     ^BB ;
:  ESP:     ^BC ; :  EBP:     ^BD ; :  ESI:     ^BE ; :  EDI:     ^BF ;
:  R8D: ^41 ^B8 ; :  R9D: ^41 ^B9 ; : R10D: ^41 ^BA ; : R11D: ^41 ^BB ;
: R12D: ^41 ^BC ; : R13D: ^41 ^BD ; : R14D: ^41 ^BE ; : R15D: ^41 ^BF ;

:  RAX: ^48 ^B8 ; :  RCX: ^48 ^B9 ; :  RDX: ^48 ^BA ; :  RBX: ^48 ^BB ;
:  RSP: ^48 ^BC ; :  RBP: ^48 ^BD ; :  RSI: ^48 ^BE ; :  RDI: ^48 ^BF ;
:   R8: ^49 ^B8 ; :   R9: ^49 ^B9 ; :  R10: ^49 ^BA ; :  R11: ^49 ^BB ;
:  R12: ^49 ^BC ; :  R13: ^49 ^BD ; :  R14: ^49 ^BE ; :  R15: ^49 ^BF ;

\ 32-bit signed immediate into 64-bit register.
: RAX:L ^48 ^C7 ^C0 ; :  R8:L ^49 ^C7 ^C0 ;
: RCX:L ^48 ^C7 ^C1 ; :  R9:L ^49 ^C7 ^C1 ;
: RDX:L ^48 ^C7 ^C2 ; : R10:L ^49 ^C7 ^C2 ;
: RBX:L ^48 ^C7 ^C3 ; : R11:L ^49 ^C7 ^C3 ;
: RSP:L ^48 ^C7 ^C4 ; : R12:L ^49 ^C7 ^C4 ;
: RBP:L ^48 ^C7 ^C5 ; : R13:L ^49 ^C7 ^C5 ;
: RSI:L ^48 ^C7 ^C6 ; : R14:L ^49 ^C7 ^C6 ;
: RDI:L ^48 ^C7 ^C7 ; : R15:L ^49 ^C7 ^C7 ;

\ zero out a 64-bit register
: RAX:0 ^31 ^C0 ; :  R8:0 ^45 ^31 ^C0 ;
: RCX:0 ^31 ^C9 ; :  R9:0 ^45 ^31 ^C9 ;
: RDX:0 ^31 ^D2 ; : R10:0 ^45 ^31 ^D2 ;
: RBX:0 ^31 ^DB ; : R11:0 ^45 ^31 ^DB ;
: RSP:0 ^31 ^E4 ; : R12:0 ^45 ^31 ^E4 ;
: RBP:0 ^31 ^ED ; : R13:0 ^45 ^31 ^ED ;
: RSI:0 ^31 ^F6 ; : R14:0 ^45 ^31 ^F6 ;
: RDI:0 ^31 ^FF ; : R15:0 ^45 ^31 ^FF ;

\ Push register on return stack.
: RAX>R     ^50 ; : RCX>R     ^51 ; : RDX>R     ^52 ; : RBX>R     ^53 ;
: RSP>R     ^54 ; : RBP>R     ^55 ; : RSI>R     ^56 ; : RDI>R     ^57 ;
\ :  R8>R ^41 ^50 ; :  R9>R ^41 ^51 ; : R10>R ^41 ^52 ; : R11>R ^41 ^53 ;
\ : R12>R ^41 ^54 ; : R13>R ^41 ^55 ; : R14>R ^41 ^56 ; : R15>R ^41 ^57 ;

\ Pop return stack to register.
: R>RAX     ^58 ; : R>RCX     ^59 ; : R>RDX     ^5A ; : R>RBX     ^5B ;
: R>RSP     ^5C ; : R>RBP     ^5D ; : R>RSI     ^5E ; : R>RDI     ^5F ;
\ : R>R8  ^41 ^58 ; : R>R9  ^41 ^59 ; : R>R10 ^41 ^5A ; : R>R11 ^41 ^5B ;
\ : R>R12 ^41 ^5C ; : R>R13 ^41 ^5D ; : R>R14 ^41 ^5E ; : R>R15 ^41 ^5F ;

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

\ Load a RIP-relative address into a register.
\ These expect an immediate signed dword offset.
: RAX:RIP+ ^48 ^8D ^05 ; \ LEA RAX, [RIP+__]

\ Subtract from RAX.
: RAX-RDX  ^48 ^2B ^C2 ; \ SUB RAX, RDX
: RAX-RBX  ^48 ^2B ^C3 ; \ SUB RAX, RBX
: RAX-RDI  ^48 ^2B ^C7 ; \ SUB RAX, RDI

\ Get user data address. An immediate byte offset is required.
\ This is assuming the user data base pointer is RBP.
: RAX:USER+ ^48 ^8D ^45 ; \ LEA RAX, [RBP+_]
: RCX:USER+ ^48 ^8D ^4D ; \ LEA RCX, [RBP+_]
: RDX:USER+ ^48 ^8D ^55 ; \ LEA RDX, [RBP+_]
: RBX:USER+ ^48 ^8D ^5D ; \ LEA RBX, [RBP+_]

: RAX>USER: ^48 ^89 ^45 ; : USER>RAX: ^48 ^8B ^45 ;
: RCX>USER: ^48 ^89 ^4D ; : USER>RCX: ^48 ^8B ^4D ;
: RDX>USER: ^48 ^89 ^55 ; : USER>RDX: ^48 ^8B ^55 ;
: RBX>USER: ^48 ^89 ^5D ; : USER>RBX: ^48 ^8B ^5D ;
: RSI>USER: ^48 ^89 ^75 ; : USER>RSI: ^48 ^8B ^75 ;
: RDI>USER: ^48 ^89 ^7D ; : USER>RDI: ^48 ^8B ^7D ;

\ User field offsets. Use in conjunction with the above.
\ Keep this in sync with "struc USER" in the ASM source.
: $USER.HERE ^00 ;
: $USER.PROG ^08 ;
: $USER.KERS ^10 ;
: $USER.KERW ^18 ;
: $USER.FORW ^20 ;
: $USER.EVAL ^28 ;
: $USER.DICT ^30 ;
: $USER.NAME ^38 ;
: $USER.FIND ^58 ;

: CALL> ( -- )
    \ Exit the current word, and compile a call to the rest
    \ of the word. The typical usage is at the start of a word
    \ definition, to turn the whole definition into a word that
    \ simply compiles a call to itself. For example,
    \
    \   : SQUARE CALL> dup * ;
    \
    \ will make it so that when SQUARE is interpreted, it will
    \ compile a call into its body (the dup * part). Thus CALL>
    \ is like the default compilation action of most forths.

    ^E8
    LEAQ< RAX, [RIP]: $00000001
    SUBQ< RAX, RDI
    STOSL
    RET

    ^E8
    R>RAX
    LEAQ< RAX, [RAX]. $-04
    SUBQ< RAX, RDI
    STOSL ;

\ Dict field offsets.
\ Keep in sync with "struc DICT" in the ASM source.
: $DICT.LINK ^00 ;
: $DICT.NAME ^08 ;
: $DICT.CODE ^28 ;
: $DICT.DATA ^30 ;

: ;
    \ Store the end address of the word in
    \ its dictionary entry, and emit RET.
    MOVQ< RAX, [RBP]. $USER.DICT
    MOVQ< RAX, [RAX]
    MOVQ> RDI, [RAX]. $DICT.DATA
    ^C3 \ RET
    ;

: INLINE> ( -- ) CALL>
    \ Changes the action of a word to be the inlining of
    \ its contents. Only do this if the word contains
    \ no referneces (e.g. CALLs) because otherwise the
    \ reference will be all wrong.
    R>RCX RSI>R
    MOVQ< RSI, RCX
    MOVQ< RCX, [RAX]. $DICT.DATA
    SUBQ< RCX, RSI
    REP MOVSB
    R>RSI
    ;

: INLINE/CALL> ( -- ) CALL>
    \ Same as INLINE> but indicates that it would
    \ be ok to call this word directly too.
    R>RCX RSI>R
    MOVQ< RSI, RCX
    MOVQ< RCX, [RAX]. $DICT.DATA
    SUBQ< RCX, RSI
    REP MOVSB
    R>RSI
    ;

: [SETUP]
    \ Set up data stack (R15).
    LEAQ< R15, [RBP]: $10000000 ;
[SETUP]

\ Basic stack words. These preserve non-stack registers (RBX, R15).
: DUP ( a -- a a ) INLINE>
    LEAQ< R15, [R15]. $F8
    MOVQ> RBX, [R15] ;
: dup ( a -- a a ) CALL>
    DUP ;

: NIP ( a b -- b ) INLINE>
    LEAQ< R15, [R15]. $08 ;
: nip ( a b -- b ) CALL>
    NIP ;

: DROP ( a -- ) INLINE>
    MOVQ< RBX, [R15]
    LEAQ< R15, [R15]. $08 ;
: drop ( a -- ) CALL>
    DROP ;

\ Deeper stack words. These can trash RAX and RDX.
: SWAP ( a b -- b a ) INLINE>
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]
    MOVQ< RBX, RAX ;
: swap ( a b -- b a ) CALL>
    SWAP ;
: ROTL ( a b c -- b c a ) INLINE>
    MOVQ< RAX, [R15]. $08
    MOVQ< RDX, [R15]
    MOVQ> RBX, [R15]
    MOVQ> RBX, [R15]. $08
    MOVQ< RBX, RAX ;
: rotl ( a b c -- b c a ) CALL>
    ROTL ;
: ROTR ( a b c -- c a b ) INLINE>
    MOVQ< RDX, [R15]. $08
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]. $08
    MOVQ> RDX, [R15]
    MOVQ< RBX, RAX ;
: rotr ( a b c -- c a b ) CALL>
    ROTR ;

\ Push and pop registers on data stack.
: RAX> INLINE> DUP MOVQ> RAX, RBX ; : >RAX INLINE> MOVQ> RBX, RAX DROP ;
: RCX> INLINE> DUP MOVQ> RCX, RBX ; : >RCX INLINE> MOVQ> RBX, RCX DROP ;
: RDX> INLINE> DUP MOVQ> RDX, RBX ; : >RDX INLINE> MOVQ> RBX, RDX DROP ;
: RSP> INLINE> DUP MOVQ> RSP, RBX ; : >RSP INLINE> MOVQ> RBX, RSP DROP ;
: RBP> INLINE> DUP MOVQ> RBP, RBX ; : >RBP INLINE> MOVQ> RBX, RBP DROP ;
: RSI> INLINE> DUP MOVQ> RSI, RBX ; : >RSI INLINE> MOVQ> RBX, RSI DROP ;
: RDI> INLINE> DUP MOVQ> RDI, RBX ; : >RDI INLINE> MOVQ> RBX, RDI DROP ;
:  R8> INLINE> DUP MOVQ>  R8, RBX ; : >R8  INLINE> MOVQ> RBX, R8  DROP ;
:  R9> INLINE> DUP MOVQ>  R9, RBX ; : >R9  INLINE> MOVQ> RBX, R9  DROP ;
: R10> INLINE> DUP MOVQ> R10, RBX ; : >R10 INLINE> MOVQ> RBX, R10 DROP ;
: R11> INLINE> DUP MOVQ> R11, RBX ; : >R11 INLINE> MOVQ> RBX, R11 DROP ;
: R12> INLINE> DUP MOVQ> R12, RBX ; : >R12 INLINE> MOVQ> RBX, R12 DROP ;
: R13> INLINE> DUP MOVQ> R13, RBX ; : >R13 INLINE> MOVQ> RBX, R13 DROP ;
: R14> INLINE> DUP MOVQ> R14, RBX ; : >R14 INLINE> MOVQ> RBX, R14 DROP ;

: RDROP ( R: x -- ) INLINE>
    \ Drop top of return stack.
    LEAQ< RSP, [0+ RSP]. $08 ;

: 0 ( -- 0 ) INLINE/CALL>
    \ Push zero on data stack.
    DUP XORL< RBX, RBX ;
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

\ syscall numbers (darwin) as 32-bit immediates
: $SYS_EXIT  ^02000001 ;
: $SYS_FORK  ^02000002 ;
: $SYS_READ  ^02000003 ;
: $SYS_WRITE ^02000004 ;
: $SYS_OPEN  ^02000005 ;
: $SYS_CLOSE ^02000006 ;
: $SYS_MMAP  ^020000C5 ;

: bye ( -- ) CALL>
    \ End the program.
    MOVL#: RAX $SYS_EXIT
    XORL< RDI, RDI
    SYSCALL ;
: [bye] bye ;

: emit ( c -- ) CALL>
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

: cr ( -- ) CALL>
    \ Emit a newline to stdout.
    'LF' emit ;
: space ( -- ) CALL>
    \ Emit a single space character to stdout.
    'BL' emit ;

: $TEMPOFFSET ^0FFFC000 ;
: RDI:TEMP ( -- RDI=TEMP ) CALL>
    \ Load temporary program memory address into RDI.
    MOVQ< RDI, [RBP]. $USER.PROG
    LEAQ< RDI, [RDI]: $TEMPOFFSET ;

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
    RDI>
    RDI:TEMP ;

: ]
    \ Finish the temporary program, restore RDI to its original
    \ location, then run the temporary program.
    ^C3
    RDI:TEMP RDI>R
    >RDI
    ;

: BRANCH-TOO-BIG CALL>
    \ TODO display message.
    MOVL#: RAX $SYS_EXIT
    MOVL#: RDI $00000005
    SYSCALL ;
: VERIFY-BRANCH-OFFSET CALL>
    CMP-RAX: $7FFFFFFF
    JLE.  $05
    BRANCH-TOO-BIG
    CMP-RAX: $80000000
    JGE.  $05
    BRANCH-TOO-BIG ;

: branch> ( -- orig ) CALL>
    \ Create blank offset for forward branch.
    \ Must be resolved by >TARGET.
    RDI>
    XORQ< RAX, RAX
    STOSL ;

: >target ( orig -- ) CALL>
    \ Resolve forward branch.
    LEAQ< RAX, [RDI]. $-04
    SUBQ< RAX, RBX
    VERIFY-BRANCH-OFFSET
    MOVL> RAX, [RBX] \ 32 bit move
    DROP ;

: target< ( -- dest ) CALL>
    \ Push current location as backward branch target.
    \ May be used by <BRANCH any number of times.
    RDI> ;
: <branch ( dest -- ) CALL>
    \ Push offset to backward branch target.
    >RAX
    RAX-RDI
    LEAQ< RAX, [RAX]. $-04
    VERIFY-BRANCH-OFFSET
    STOSL ;

: AHEAD ( CT: -- orig ) ( RT: *a -- *a / *b )
    \ Always branches forward.
    ^E9 branch> ;
: ?IF ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Non-destructive IF. Takes the first branch
    \ if top of stack is nonzero, otherwise takes
    \ the second branch. Leaves scrutinee on stack.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^84 branch> ; \ JZ branch
: ?IF0 ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Opposite of ?IF -- takes the first branch
    \ if top of stack is zero, otherwise takes
    \ the second branch.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^85 branch> ; \ JNZ branch
: ?IF<0 ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Branch on sign. Takes the first branch if
    \ the sign is negative, otherwise takes the
    \ second branch.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^89 branch> ; \ JNS branch
: ?IF>0 ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Takes the first branch if the top of stack is
    \ positive, i.e. greater than zero. Otherwise
    \ takes the second branch.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^8E branch> ; \ JLE branch
: THEN ( CT: orig -- ) ( RT: *a / *a -- *a )
    \ Resolve a forward branch.
    >target ;
: ELSE ( CT: orig1 -- orig2 ) ( RT: *a / *b -- *b / *a )
    \ Swap between branches.
    ^E9 branch> swap >target ;

: BEGIN ( CT: -- dest ) ( RT: *a -- ~*a / *a )
    \ Begin a loop.
    target< ;
: AGAIN ( CT: dest -- ) ( RT: *~a / *a -- *b )
    \ Loop forever.
    \ Used at end of loop, e.g. BEGIN ... AGAIN
    ^E9 <branch ;
: ?UNTIL ( CT: dest -- ) ( RT: *~a x / *a x -- *a x )
    \ Keep going until nonzero. Nondestructive.
    \ Used at end of loop, e.g. BEGIN ... ?UNTIL
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^84 <branch ; \ JZ branch
: ?UNTIL0 ( CT: dest -- ) ( RT: *~a x / *a x -- *a x )
    \ Keep going until zero. Nondestructive.
    \ Used at end of loop, e.g. BEGIN ... ?UNTIL0
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^85 <branch ; \ JZ branch
: ?UNTIL>0 ( CT: dest -- ) ( RT: *~a x / *a x -- *a x )
    \ Keep going until positive. Nondestructive.
    \ Used at end of loop, e.g. BEGIN ... ?UNTIL>0
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^8E <branch ; \ JLE branch
: ?UNTIL<0 ( CT: dest -- ) ( RT: *~a x / *a x -- *a x )
    \ Keep going until negative. Nondestructive.
    \ Used at end of loop, e.g. BEGIN ... ?UNTIL<0
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^0F ^85 <branch ; \ JNS branch

\ The WHILE* have stack effects:
\   ( CT: dest -- orig dest ) ( RT: ~*a / *b -- *b / ~*a / *b )
: WHILENO ^0F ^80 branch> swap ;
: WHILEO  ^0F ^81 branch> swap ;
: WHILEAE ^0F ^82 branch> swap ;
: WHILEB  ^0F ^83 branch> swap ;
: WHILENZ ^0F ^84 branch> swap ;
: WHILEZ  ^0F ^85 branch> swap ;
: WHILEA  ^0F ^86 branch> swap ;
: WHILEBE ^0F ^87 branch> swap ;
: WHILENS ^0F ^88 branch> swap ;
: WHILES  ^0F ^89 branch> swap ;
: WHILEPO ^0F ^8A branch> swap ;
: WHILEPE ^0F ^8B branch> swap ;
: WHILEGE ^0F ^8C branch> swap ;
: WHILEL  ^0F ^8D branch> swap ;
: WHILEG  ^0F ^8E branch> swap ;
: WHILELE ^0F ^8F branch> swap ;
: REPEAT ( CT: orig dest -- ) ( RT: *b / *~a / *a -- *b )
    ^E9 <branch >target ;

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

: /MOD ( i1 +i2 -- i3 i4 ) INLINE>
    \ Signed integer division.
    MOVQ< RAX, [R15]
    CQO
    IDIVQ_ RBX
    MOVQ> RAX, [R15]
    MOVQ< RBX, RDX ;
: /mod ( i1 +i2 -- i3 i4 ) CALL> /MOD ;

: U/MOD ( u1 +u2 -- u3 u4 ) INLINE>
    \ Unsigned integer division.
    MOVQ< RAX, [R15]
    XORL< RDX, RDX
    DIVQ_ RBX
    MOVQ> RAX, [R15]
    MOVQ< RBX, RDX ;
: u/mod ( u1 +u2 -- u3 u4 ) CALL> U/MOD ;

: NEGATE ( i1 -- i2 ) INLINE>
    NEGQ_ RBX ;
: negate ( i1 -- i2 ) INLINE/CALL>
    NEGATE ;

: .digits ( u -- ) CALL>
    \ Print digits of
    U. $0A U/MOD SWAP
    ?IF .digits ELSE DROP THEN
    '0' + emit ;

: . ( i -- ) CALL>
    \ Print decimal number, followed by space.
    ?IF<0
        '-' emit
        negate
    THEN
    .digits
    space ;

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

: main CALL>
    'H' emit 'e' emit 'l' emit 'l' emit 'o' emit '!' emit cr
    U. $0A . cr
    I8_MAX . cr
    I8_MIN . cr
    I16_MAX . cr
    I16_MIN . cr
    I32_MAX . cr
    I32_MIN . cr
    I64_MAX . cr
    I64_MIN . cr

    ;

[ main bye ]

