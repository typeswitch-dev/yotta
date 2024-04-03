[KERNEL-DEFINITIONS]

: ;
    \ Store the end address of the word in
    \ its dictionary entry, and emit RET.
    \ Trashes RAX.
    MOVQ< RAX, [RBP]: $USER.DICT
    MOVQ< RAX, [RAX]
    MOVQ> RDI, [RAX]. $DICT.DATA
    ^C3 \ RET
    ;

: INLINE> ( -- ) CALL>
    \ Changes the action of a word to be the inlining of
    \ its contents. Only do this if the word contains
    \ no referneces (e.g. CALLs) because otherwise the
    \ reference will be wrong.
    MOVQ< RDX, RSI
    POPQ# RSI
    MOVQ< RCX, [R13]. $DICT.DATA
    SUBQ< RCX, RSI
    REP MOVSB
    MOVQ< RSI, RDX
    ;

: INLINE/CALL> ( -- ) CALL>
    \ Same as INLINE> but indicates that it would
    \ be ok to call this word directly too.
    MOVQ< RDX, RSI
    POPQ# RSI
    MOVQ< RCX, [R13]. $DICT.DATA
    SUBQ< RCX, RSI
    REP MOVSB
    MOVQ< RSI, RDX
    ;

: KWORD ( "<spaces>name" -- ) CALL>
    \ Parse word and fill name buffer.
    \ Relies on the engine's WORD primitive.
    CALLQ_ [RBP]: $USER.WORD ;

: KFIND ( -- RAX=dict|0 ) CALL>
    \ Search for word in name buffer, among
    \ all the wordlists in the search order.
    \ Returns the first matching dictionary entry in RAX,
    \ if found, else returns 0 in RAX.
    \ Also sets ZF based on result.
    CALLQ_ [RBP]: $USER.FIND ;

: [SETUP]
    \ Set up data stack (R15).
    LEAQ< R15, [RBP]: $10000000
    ;
[SETUP]

[FORTH-DEFINITIONS]

\ Simpler stack words. These preserve non-stack registers (RBX, R15).
: DUP ( a -- a a ) INLINE/CALL>
    LEAQ< R15, [R15]. $F8
    MOVQ> RBX, [R15] ;
: NIP ( a b -- b ) INLINE/CALL>
    LEAQ< R15, [R15]. $08 ;
: DROP ( a -- ) INLINE/CALL>
    MOVQ< RBX, [R15]
    LEAQ< R15, [R15]. $08 ;
: OVER ( a b -- a b a ) INLINE/CALL>
    DUP
    MOVQ< RBX, [R15]. $08 ;
: 2DUP ( a b -- a b a b ) INLINE/CALL>
    OVER OVER ;
: 2DROP ( a b -- ) INLINE/CALL>
    MOVQ< RBX, [R15]. $08
    LEAQ< R15, [R15]. $10 ;

\ Deeper stack words. These can trash RAX and RDX.
: SWAP ( a b -- b a ) INLINE/CALL>
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]
    MOVQ< RBX, RAX ;
: ROTL ( a b c -- b c a ) INLINE/CALL>
    MOVQ< RAX, [R15]. $08
    MOVQ< RDX, [R15]
    MOVQ> RBX, [R15]
    MOVQ> RBX, [R15]. $08
    MOVQ< RBX, RAX ;
: ROTR ( a b c -- c a b ) INLINE/CALL>
    MOVQ< RDX, [R15]. $08
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]. $08
    MOVQ> RDX, [R15]
    MOVQ< RBX, RAX ;
: TUCK ( a b -- b a b ) INLINE/CALL>
    MOVQ< RAX, [R15]
    MOVQ> RBX, [R15]
    LEAQ< R15, [R15]. $F8
    MOVQ> RAX, [R15] ;

: >R ( x -- ) ( R: -- x ) INLINE>
    PUSHQ# RBX
    DROP ;
: 2>R ( x1 x2 -- ) ( R: -- x1 x2 ) INLINE>
    PUSHQ_ [R15]
    PUSHQ# RBX
    MOVQ< RBX, [R15]. $08
    LEAQ< R15, [R15]. $10 ;
: R> ( R: x -- ) ( -- x ) INLINE>
    DUP
    POPQ# RBX ;
: 2R> ( R: x1 x2 -- ) ( -- x1 x2 ) INLINE>
    MOVQ> RBX, [R15]. $F8
    LEAQ< R15, [R15]. $F0
    POPQ# RBX
    POPQ_ [R15]
    ;
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

( : 'NUL' INLINE/CALL>     0  ; ) : 'BL'  INLINE/CALL> U. $20 ;
\ : 'SOH' INLINE/CALL> U. $01 ; \ : '!'   INLINE/CALL> U. $21 ;
\ : 'STX' INLINE/CALL> U. $02 ; \ : '"'   INLINE/CALL> U. $22 ;
\ : 'ETX' INLINE/CALL> U. $03 ; \ : '#'   INLINE/CALL> U. $23 ;
\ : 'SOT' INLINE/CALL> U. $04 ; \ : '$'   INLINE/CALL> U. $24 ;
\ : 'ENQ' INLINE/CALL> U. $05 ; \ : '%'   INLINE/CALL> U. $25 ;
\ : 'ACK' INLINE/CALL> U. $06 ; \ : '&'   INLINE/CALL> U. $26 ;
\ : 'BEL' INLINE/CALL> U. $07 ; \ : '''   INLINE/CALL> U. $27 ;
\ : 'BS'  INLINE/CALL> U. $08 ; \ : '('   INLINE/CALL> U. $28 ;
\ : 'HT'  INLINE/CALL> U. $09 ; \ : ')'   INLINE/CALL> U. $29 ;
  : 'LF'  INLINE/CALL> U. $0A ; \ : '*'   INLINE/CALL> U. $2A ;
\ : 'VT'  INLINE/CALL> U. $0B ; \ : '+'   INLINE/CALL> U. $2B ;
\ : 'FF'  INLINE/CALL> U. $0C ; \ : ','   INLINE/CALL> U. $2C ;
( : 'CR'  INLINE/CALL> U. $0D ; ) : '-'   INLINE/CALL> U. $2D ;
\ : 'SO'  INLINE/CALL> U. $0E ; \ : '.'   INLINE/CALL> U. $2E ;
\ : 'SI'  INLINE/CALL> U. $0F ; \ : '/'   INLINE/CALL> U. $2F ;
( : 'DLE' INLINE/CALL> U. $10 ; ) : '0'   INLINE/CALL> U. $30 ;
\ : 'DC1' INLINE/CALL> U. $11 ; \ : '1'   INLINE/CALL> U. $31 ;
\ : 'DC2' INLINE/CALL> U. $12 ; \ : '2'   INLINE/CALL> U. $32 ;
\ : 'DC3' INLINE/CALL> U. $13 ; \ : '3'   INLINE/CALL> U. $33 ;
\ : 'DC4' INLINE/CALL> U. $14 ; \ : '4'   INLINE/CALL> U. $34 ;
\ : 'NAK' INLINE/CALL> U. $15 ; \ : '5'   INLINE/CALL> U. $35 ;
\ : 'SYN' INLINE/CALL> U. $16 ; \ : '6'   INLINE/CALL> U. $36 ;
\ : 'ETB' INLINE/CALL> U. $17 ; \ : '7'   INLINE/CALL> U. $37 ;
\ : 'CAN' INLINE/CALL> U. $18 ; \ : '8'   INLINE/CALL> U. $38 ;
\ : 'EM'  INLINE/CALL> U. $19 ; \ : '9'   INLINE/CALL> U. $39 ;
\ : 'SUB' INLINE/CALL> U. $1A ; \ : ':'   INLINE/CALL> U. $3A ;
\ : 'ESC' INLINE/CALL> U. $1B ; \ : ';'   INLINE/CALL> U. $3B ;
\ : 'FS'  INLINE/CALL> U. $1C ; \ : '<'   INLINE/CALL> U. $3C ;
\ : 'GS'  INLINE/CALL> U. $1D ; \ : '='   INLINE/CALL> U. $3D ;
\ : 'RS'  INLINE/CALL> U. $1E ; \ : '>'   INLINE/CALL> U. $3E ;
\ : 'US'  INLINE/CALL> U. $1F ; \ : '?'   INLINE/CALL> U. $3F ;

\ : '@'   INLINE/CALL> U. $40 ; \ : '`'   INLINE/CALL> U. $60 ;
\ : 'A'   INLINE/CALL> U. $41 ; \ : 'a'   INLINE/CALL> U. $61 ;
\ : 'B'   INLINE/CALL> U. $42 ; \ : 'b'   INLINE/CALL> U. $62 ;
\ : 'C'   INLINE/CALL> U. $43 ; \ : 'c'   INLINE/CALL> U. $63 ;
\ : 'D'   INLINE/CALL> U. $44 ; \ : 'd'   INLINE/CALL> U. $64 ;
\ : 'E'   INLINE/CALL> U. $45 ; \ : 'e'   INLINE/CALL> U. $65 ;
\ : 'F'   INLINE/CALL> U. $46 ; \ : 'f'   INLINE/CALL> U. $66 ;
\ : 'G'   INLINE/CALL> U. $47 ; \ : 'g'   INLINE/CALL> U. $67 ;
\ : 'H'   INLINE/CALL> U. $48 ; \ : 'h'   INLINE/CALL> U. $68 ;
\ : 'I'   INLINE/CALL> U. $49 ; \ : 'i'   INLINE/CALL> U. $69 ;
\ : 'J'   INLINE/CALL> U. $4A ; \ : 'j'   INLINE/CALL> U. $6A ;
\ : 'K'   INLINE/CALL> U. $4B ; \ : 'k'   INLINE/CALL> U. $6B ;
\ : 'L'   INLINE/CALL> U. $4C ; \ : 'l'   INLINE/CALL> U. $6C ;
\ : 'M'   INLINE/CALL> U. $4D ; \ : 'm'   INLINE/CALL> U. $6D ;
\ : 'N'   INLINE/CALL> U. $4E ; \ : 'n'   INLINE/CALL> U. $6E ;
\ : 'O'   INLINE/CALL> U. $4F ; \ : 'o'   INLINE/CALL> U. $6F ;
\ : 'P'   INLINE/CALL> U. $50 ; \ : 'p'   INLINE/CALL> U. $70 ;
\ : 'Q'   INLINE/CALL> U. $51 ; \ : 'q'   INLINE/CALL> U. $71 ;
\ : 'R'   INLINE/CALL> U. $52 ; \ : 'r'   INLINE/CALL> U. $72 ;
\ : 'S'   INLINE/CALL> U. $53 ; \ : 's'   INLINE/CALL> U. $73 ;
\ : 'T'   INLINE/CALL> U. $54 ; \ : 't'   INLINE/CALL> U. $74 ;
\ : 'U'   INLINE/CALL> U. $55 ; \ : 'u'   INLINE/CALL> U. $75 ;
\ : 'V'   INLINE/CALL> U. $56 ; \ : 'v'   INLINE/CALL> U. $76 ;
\ : 'W'   INLINE/CALL> U. $57 ; \ : 'w'   INLINE/CALL> U. $77 ;
\ : 'X'   INLINE/CALL> U. $58 ; \ : 'x'   INLINE/CALL> U. $78 ;
\ : 'Y'   INLINE/CALL> U. $59 ; \ : 'y'   INLINE/CALL> U. $79 ;
\ : 'Z'   INLINE/CALL> U. $5A ; \ : 'z'   INLINE/CALL> U. $7A ;
\ : '['   INLINE/CALL> U. $5B ; \ : '{'   INLINE/CALL> U. $7B ;
\ : '\'   INLINE/CALL> U. $5C ; \ : '|'   INLINE/CALL> U. $7C ;
\ : ']'   INLINE/CALL> U. $5D ; \ : '}'   INLINE/CALL> U. $7D ;
\ : '^'   INLINE/CALL> U. $5E ; \ : '~'   INLINE/CALL> U. $7E ;
\ : '_'   INLINE/CALL> U. $5F ; \ : 'DEL' INLINE/CALL> U. $7F ;

: CR ( -- ) CALL>
    \ Emit a newline to stdout.
    'LF' EMIT ;
: SPACE ( -- ) CALL>
    \ Emit a single space character to stdout.
    'BL' EMIT ;

: NAME-NOT-FOUND CALL>
    \ TODO display message.
    MOVL#: RAX $SYS_EXIT
    MOVL#: RDI $0000000D \ exit code = 13
    SYSCALL ;

: NT' ( -- nt ) CALL>
    DUP
    KWORD KFIND
    MOVQ< RBX, RAX
    TESTQ> RAX, RAX
    JNZ. $05
    NAME-NOT-FOUND
    ;

: POSTPONE
    NT'
    ^49 ^55         \ PUSH R13
    ^49 ^BD         \ MOV R13, ___
    MOVQ< RAX, RBX
    STOSQ
    ^41 ^FF ^65     \ CALL [R13 + $DICT.CODE]
    MOVB#. RAX $DICT.CODE
    STOSB
    ^49 ^5D         \ POP R13
    DROP ;

: LITERAL
    POSTPONE DUP
    \ TODO emit nicer code for smaller literals
    ^48 ^BB
    MOVQ< RAX, RBX
    STOSQ
    DROP
    ;

: ^VARIABLE
    ^B8
    MOVQ< RAX, [RBP]: $USER.HERE
    SUBQ< RAX, RBP
    STOSL
    ^AB
    ADDQ_. [RBP]: $USER.HERE $08
    ;

: $FOO ^VARIABLE ;

: [NT'] NT' POSTPONE LITERAL ;

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
    MOVL#: RDI $00000011
    SYSCALL ;
: VERIFY-BRANCH-OFFSET ( -- ) CALL>
    \ Verify that branch offset in RAX fits in a signed byte.
    \ Otherwise panics with a BRANCH-TOO-BIG error.
    CMP-RAX: $0000007F
    JLE.  $05
    BRANCH-TOO-BIG
    CMP-RAX: $FFFFFF80
    JGE.  $05
    BRANCH-TOO-BIG ;

[ASSEMBLER-DEFINITIONS]

: BRANCH> ( -- orig ) CALL>
    \ Create blank offset for forward branch.
    \ Must be resolved by >TARGET.
    DUP
    MOVQ< RBX, RDI
    XORQ< RAX, RAX
    STOSB ;
: >TARGET ( orig -- ) CALL>
    \ Resolve forward branch.
    LEAQ< RAX, [RDI]. $FF
    SUBQ< RAX, RBX
    VERIFY-BRANCH-OFFSET
    MOVB> RAX, [RBX]
    DROP ;

: TARGET< ( -- dest ) CALL>
    \ Push current location as backward branch target.
    \ May be used by <BRANCH any number of times.
    DUP
    MOVQ< RBX, RDI ;
: <BRANCH ( dest -- ) CALL>
    \ Push offset to backward branch target.
    SUBQ< RBX, RDI
    LEAQ< RAX, [RBX]. $FF
    VERIFY-BRANCH-OFFSET
    STOSB
    DROP ;

[FORTH-DEFINITIONS]

: AHEAD ( CT: -- orig ) ( RT: *a -- *a / *b )
    \ Always branches forward.
    ^EB BRANCH> ;
: ?IF ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Non-destructive IF. Takes the first branch
    \ if top of stack is nonzero, otherwise skips
    \ to THEN or ELSE.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^74 BRANCH> ; \ JZ branch
: <IF ( CT: -- orig ) ( RT: *a x -- *a x / *a x )
    \ Non-destructive IF. Takes the first branch
    \ if top of stack is negative, otherwise skips
    \ to THEN or ELSE.
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^79 BRANCH> ; \ JNS branch

: THEN ( CT: orig -- ) ( RT: *a / *a -- *a )
    \ Resolve a forward branch.
    >TARGET ;
: ELSE ( CT: orig1 -- orig2 ) ( RT: *a / *b -- *b / *a )
    \ Swap between branches.
    ^EB BRANCH> SWAP >TARGET ;

: BEGIN ( CT: -- dest ) ( RT: *a -- ~*a / *a )
    \ Begin a loop.
    TARGET< ;
: AGAIN ( CT: dest -- ) ( RT: *~a / *a -- *b )
    \ Loop forever.
    \ Used at end of loop, e.g. BEGIN ... AGAIN
    ^EB <BRANCH ;
: ?UNTIL ( CT: dest -- ) ( RT: *~a x / *a x -- *a x )
    \ Keep going until nonzero. Nondestructive.
    \ Used at end of loop, e.g. BEGIN ... ?UNTIL
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^74 <BRANCH ; \ JZ branch

: ?WHILE ( CT: dest -- orig dest ) ( RT: ~*a / *b x -- *b x / ~*a / *b x )
    \ Keep going while value is nonzero. Nondestructive.
    \ Typical usage: begin ... ?while ... repeat
    ^48 ^85 ^DB \ TEST RBX, RBX
    ^74 BRANCH> ; \ JNZ branch
    SWAP ;

: REPEAT ( CT: orig dest -- ) ( RT: *b / *~a / *a -- *b )
    \ End a "begin ... while ..." loop.
    \ This is equivalent to "again then".
    ^EB <BRANCH >TARGET ;

[ASSEMBLER-DEFINITIONS]

\ Like ?IF but use CPU flags instead of testing stack top.
: IFNO ^70 BRANCH> ; \ if no overflow      (OF=0)
: IFO  ^71 BRANCH> ; \ if overflow         (OF=1)
: IFAE ^72 BRANCH> ; \ if above or equal   (CF=0)
: IFB  ^73 BRANCH> ; \ if below            (CF=1)
: IFNZ ^74 BRANCH> ; \ if non-equal        (ZF=1)
: IFZ  ^75 BRANCH> ; \ if zero             (ZF=0)
: IFA  ^76 BRANCH> ; \ if above            (CF=0 and ZF=0)
: IFBE ^77 BRANCH> ; \ if below or equal   (CF=1 or  ZF=1)
: IFNS ^78 BRANCH> ; \ if positive         (SF=0)
: IFS  ^79 BRANCH> ; \ if negative         (SF=1)
: IFPO ^7A BRANCH> ; \ if parity odd       (PF=0)
: IFPE ^7B BRANCH> ; \ if parity even      (PF=1)
: IFGE ^7C BRANCH> ; \ if greater or equal (SF=OF)
: IFL  ^7D BRANCH> ; \ if less             (SF<>OF)
: IFG  ^7E BRANCH> ; \ if greater          (SF=OF)
: IFLE ^7F BRANCH> ; \ if less or equal    (SF<>OF)

\ Like ?UNTIL but using CPU flags instead of testing stack top.
: UNTILNO ^70 <BRANCH ; \ until no overflow      (OF=0)
: UNTILO  ^71 <BRANCH ; \ until overflow         (OF=1)
: UNTILAE ^72 <BRANCH ; \ until above or equal   (CF=0)
: UNTILB  ^73 <BRANCH ; \ until below            (CF=1)
: UNTILNZ ^74 <BRANCH ; \ until non-equal        (ZF=1)
: UNTILZ  ^75 <BRANCH ; \ until zero             (ZF=0)
: UNTILA  ^76 <BRANCH ; \ until above            (CF=0 and ZF=0)
: UNTILBE ^77 <BRANCH ; \ until below or equal   (CF=1 or  ZF=1)
: UNTILNS ^78 <BRANCH ; \ until positive         (SF=0)
: UNTILS  ^79 <BRANCH ; \ until negative         (SF=1)
: UNTILPO ^7A <BRANCH ; \ until parity odd       (PF=0)
: UNTILPE ^7B <BRANCH ; \ until parity even      (PF=1)
: UNTILGE ^7C <BRANCH ; \ until greater or equal (SF=OF)
: UNTILL  ^7D <BRANCH ; \ until less             (SF<>OF)
: UNTILG  ^7E <BRANCH ; \ until greater          (SF=OF)
: UNTILLE ^7F <BRANCH ; \ until less or equal    (SF<>OF)

\ Like ?WHILE but uses CPU flags instead of testing stack top.
: WHILENO ^70 BRANCH> SWAP ; \ while no overflow      (OF=0)
: WHILEO  ^71 BRANCH> SWAP ; \ while overflow         (OF=1)
: WHILEAE ^72 BRANCH> SWAP ; \ while above or equal   (CF=0)
: WHILEB  ^73 BRANCH> SWAP ; \ while below            (CF=1)
: WHILENZ ^74 BRANCH> SWAP ; \ while non-equal        (ZF=1)
: WHILEZ  ^75 BRANCH> SWAP ; \ while zero             (ZF=0)
: WHILEA  ^76 BRANCH> SWAP ; \ while above            (CF=0 and ZF=0)
: WHILEBE ^77 BRANCH> SWAP ; \ while below or equal   (CF=1 or  ZF=1)
: WHILENS ^78 BRANCH> SWAP ; \ while positive         (SF=0)
: WHILES  ^79 BRANCH> SWAP ; \ while negative         (SF=1)
: WHILEPO ^7A BRANCH> SWAP ; \ while parity odd       (PF=0)
: WHILEPE ^7B BRANCH> SWAP ; \ while parity even      (PF=1)
: WHILEGE ^7C BRANCH> SWAP ; \ while greater or equal (SF=OF)
: WHILEL  ^7D BRANCH> SWAP ; \ while less             (SF<>OF)
: WHILEG  ^7E BRANCH> SWAP ; \ while greater          (SF=OF)
: WHILELE ^7F BRANCH> SWAP ; \ while less or equal    (SF<>OF)

[FORTH-DEFINITIONS]

: + ( n1 n2 -- n3 ) CALL>
    \ Addition.
    ADDQ< RBX, [R15]
    NIP ;
: - ( n1 n2 -- n3 ) CALL>
    \ Subtraction.
    MOVQ< RAX, [R15]
    NIP
    SUBQ< RAX, RBX
    MOVQ< RBX, RAX ;
: * ( n1 n2 -- n3 ) CALL>
    \ Multiplication.
    IMULQ< RBX, [R15]
    NIP ;
: < ( n1 n2 -- flag ) CALL>
    SUBQ< RBX, [R15]
    SARQ_. RBX $3F
    NIP ;
: > ( n1 n2 -- flag ) CALL>
    SUBQ< RBX, [R15]
    SARQ_. RBX $3F
    NOTQ_ RBX
    NIP ;
: = ( n1 n2 -- flag ) CALL>
    CMPQ< RBX, [R15]
    SETNZ_ RBX
    DECB_ RBX
    MOVSXQB< RBX, RBX
    NIP ;

: /MOD ( i1 +i2 -- i3 i4 ) CALL>
    \ Signed integer division.
    MOVQ< RAX, [R15]
    CQO
    IDIVQ_ RBX
    MOVQ> RAX, [R15]
    MOVQ< RBX, RDX ;
: U/MOD ( u1 +u2 -- u3 u4 ) CALL>
    \ Unsigned integer division.
    MOVQ< RAX, [R15]
    XORL< RDX, RDX
    DIVQ_ RBX
    MOVQ> RAX, [R15]
    MOVQ< RBX, RDX ;

: 1+ ( n1 -- n2 ) INLINE/CALL>
    INCQ_ RBX ;
: 1- ( n1 -- n2 ) INLINE/CALL>
    DECQ_ RBX ;

: NEGATE ( i1 -- i2 ) INLINE/CALL>
    NEGQ_ RBX ;
: INVERT ( i1 -- i2 ) INLINE/CALL>
    NOTQ_ RBX ;

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

: FIND-NAME-IN ( c-addr u wid -- nt|0 ) CALL>
    DROP DROP DROP 0 ;

: EXIT ^C3 ;

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
