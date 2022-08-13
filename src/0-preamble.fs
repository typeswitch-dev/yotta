: ; ^C3 ;
: SKIP-UNTIL ^AC ^3A ^C2 ^75 ^FB ;
: \ $B2 $0A SKIP-UNTIL ;
: ( $B2 $29 SKIP-UNTIL ;

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
: $USER.WORD ^00000070 ; \ "WORD" primitive
: $USER.FIND ^00000078 ; \ "FIND" primitive
: $USER.SORD ^00000080 ;

\ Dict field offsets. Keep in sync with "struc DICT" in the ASM source.
\ The dictionary is a linked list of definitions.
: $DICT.LINK ^00 ; \ Link to previous dictionary entry.
: $DICT.NAME ^08 ; \ Length byte + 31 bytes of name data, padded with spaces.
: $DICT.CODE ^28 ; \ Pointer to code that will get executed when word is read.
: $DICT.DATA ^30 ; \ Extra data associated with the word.

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
    \ Using CALL> in the assembler saves us about 3KB so it's
    \ worth it to define this directly in machine code before
    \ we have the assembler up and running.

    ^E8                     \ emit E8 byte ( E8 xx xx xx xx = CALL NEAR )
    $48 $8D $05 $00000001   \ LEA RAX, [RIP+1]
    $48 $2B $C7             \ SUB RAX, RDI
    $AB                     \ STOSL
    $C3                     \ RET

    ^E8                     \ emit E8 byte
    $58                     \ POP RAX
    $48 $8D $40 $FC         \ LEA RAX, [RAX-4]
    $48 $2B $C7             \ SUB RAX, RDI
    $AB                     \ STOSL
    ;

: SET-CURRENT CALL>
    \ Change current dictionary based on offset to RBP.
    $48 $03 $C5            \ ADD RAX, RBP
    $48 $89 $85 $USER.DICT \ MOV [RBP+USER.DICT], RAX
    ;

: [KERNEL-DEFINITIONS] ( -- )
    \ Change current dictionary to KERNEL.
    $B8 $USER.KERW SET-CURRENT ;

: [MIRTH-DEFINITIONS] ( -- )
    \ Change current dictionary to MIRTH.
    $B8 $USER.MIRW SET-CURRENT ;

: [FORTH-DEFINITIONS] ( -- )
    \ Change current dictionary to FORTH.
    $B8 $USER.FORW SET-CURRENT ;

: [ASSEMBLER-DEFINITIONS] ( -- )
    \ Change current dictionary to ASSEMBLER.
    $B8 $USER.ASMW SET-CURRENT ;

: [MINIMAL-DEFINITIONS] ( -- )
    \ Change current dictionary to MINIMAL.
    $B8 $USER.MINW SET-CURRENT ;
