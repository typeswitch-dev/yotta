:\ ^AC ^3C0A ^75FB ^C3      \ Implement backslash comments like this.
:( ^AC ^3C29 ^75FB ^C3      ( Implement parentheses comments like this. )

\ Welcome to nanoforth! This is a tiny tiny tiny version of forth that
\ has only two built-in instructions:
\
\       ^XX         Compile byte XX into program memory. XX is specified
\                   with uppercase hex digits. For example, ^C3 will
\                   compile the byte C3 into memory, which represents the
\                   RET opcode in the x86-64 instruction set. To improve
\                   readability, you can specify more than one byte at a
\                   time. Bytes are compiled one at a time, left to right.
\
\       :A          Define label A, where A is any single byte. The label
\                   is associated with the current location in program
\                   memory. Once a label is defined, you can CALL the
\                   location associated with this label by simply using
\                   the label's name (in this case, the A byte). Labels
\                   can be redefined without affecting existing code.
\                   Note that bytes 1 through 32 are ignored, and byte 0
\                   is special (it is used when an undefined label is
\                   invoked).
\
\ As an example of these two instructions in action, see the first two
\ lines above, where we define the \ and ( labels. These are used to
\ implement comments. Translating the first definition, we have:
\
\       :\                              define label \
\           ^AC         LODSB           load source byte into AL
\           ^3C0A       CMP AL, 10      compare AL against newline '\n'
\           ^75FB       JNE -5          jump back if they are not equal
\           ^C3         RET             return from definition
\
\ And ( is similar but it looks for a right parenthesis instead of a newline:
\
\       :(                              define label (
\           ^AC         LODSB           load source byte into AL
\           ^3C0A       CMP AL, 41      compare AL against right paren ')'
\           ^75FB       JNE -5          jump back if they are not equal
\           ^C3         RET             return from definition
\
\ Since we are working directly with machine code, we need to know how
\ nanoforth sets up the machine. There are three regions of memory that
\ are important for nanoforth, and a pointer for each. These regions
\ are the source code, the program memory, and the dictionary.
\
\ The source code comes from this file, and it is compiled directly into
\ into the .text section of the nanoforth binary. This means the source
\ code is readonly. The current location in the source code is maintained
\ in the register RSI. You can implement new syntax by manipulating RSI,
\ which is what \ and ( above do.
\
\ The program memory is a big chunk of "Read-Write-Execute" (RWX) memory
\ allocated by nanoforth, for the purposes of building a program that we
\ can also execute on-the-fly. This is where the ^XX instruction outputs,
\ its bytes, and where :A defines labels as pointing to. The current
\ location in program memory is maintained in the register RDI. You can
\ implement new control flow primitives by manipulating RDI, but also
\ you will be constantly using RDI simply to emit the instructions and
\ define new words as needed.
\
\ Finally, the dictionary is a region of memory reserved for the program
\ to also manipulate. It is holds the dictionary, which is an array of
\ 256 pointers into program memory. Each pointer holds the location of
\ the corresponding byte. E.g. the label 'a' corresponds to the 97th
\ pointer in this array, since 'a' is ASCII 97. If the label has not
\ been defined, the pointer is NULL. The dictionary consists of only
\ these 256 pointers, but the region reserved for the dictionary is
\ much larger (0x10000000 bytes), and you can use the additional memory
\ as you see fit. Furthermore, the pointers 1 through 32 are not used
\ by nanoforth since those bytes are skipped by design, so you use these
\ however you'd like. The dictionary pointer is held in the R15 register.
\
\ As you'll see below, we actually define an additional "dictionary"
\ of 256 pointers, right after the first one. We use this dictionary
\ for "compile-mode", a variation on the original mode that is to be
\ used when compiling definitions. The way we enter "compile-mode" is
\ by changing R15 to point this new dictionary. This is a useful trick
\ in general, for implementing new "modes".
\
\ So to summarize, as far as the nanoforth engine is concerned, we have:
\
\       RSI = source code pointer
\       RDI = program memory pointer
\       R15 = dictionary pointer
\
\ And that's all that nanoforth originally cares about. Oh, additionally,
\ RSP is the usual machine stack which is used for CALL and RET, so you
\ can think of RSP as the return stack pointer.
\
\ Now to actually write some definitions that work well together, we need
\ somewhere to pass data. The usual approach is to have a separate data
\ stack and pass data on the stack. But for nanoforth, I want to just
\ forego the separate stack. We have a single stack -- the combined
\ return and data stack -- and we have to manage whether a value or
\ a return address is passed. To keep things simple, words never need to
\ take input or output on the stack, unless they are variadic. Instead
\ they take their input in registers, and leave their output in registers.
\
\ We could specify a uniform order for taking inputs & outputs in registers,
\ but this actually results in a lot of unnecessary register moves. Instead,
\ we consider certain types of values "sticky" to certain registers, and we
\ are careful not to trash registers we don't need. In particular the only
\ registers we can freely trash are RAX and RDX. Everything else must be
\ handled with care.
\
\ In general then, the "input--output" diagrams should specify which
\ register each value is passed in. In general, we use the following
\ assignments:
\
\       RAX = trash / accumulator
\       RDX = trash / remainder
\       RCX = counter, loop index
\       RBX = function pointer / branch target
\       RSI = source address
\       RDI = target address
\       R15 = dictionary / lookup table
\
\ Additionally, any word that calls an external function or syscall,
\ directly or indirectly, expects a 16-byte alignment before the
\ call, and so should maintain this 16-byte alignment when calling
\ another such word (or performing the external call).
\
\ Note that this scheme includes the registers identified above as
\ part of this mechanism. I.e. we can temporarily replace the
\ nanoforth registers with other values, in order to reuse some of
\ the functions we use in nanoforth.

:;  ( RDI -- RDI+ RAX~ )          \ "exit"
    \ Compile RET into target.
    ^B0C3           \ MOV AL, C3        ( note: C3 = RET )
    ^AA             \ STOSB
    ^C3             \ RET

:'  ( R15 RSI:c -- R15 RSI+ RBX=R15[c] ) \ "quote"
    \ Lookup next character in dictionary.
    \ Usage:    '(
    ^31C0           \ XOR EAX, EAX              ( RAX = 0 )
    ^AC             \ LODSB                     ( RAX = label byte )
    ^498B1CC7       \ MOV RBX, [R15 + RAX*8]    ( RBX = label addr )
    ;

:O  ( R15 RDI RBX -- R15 RDI+4 RBX ) \ "offset"
    \ Emit 4-byte relative offset for address in RBX.
    ^488D43FC       \ LEA RAX, [RBX - 4]        ( RAX = RBX - 4 )
    ^482BC7         \ SUB RAX, RDI              ( RAX = RBX - RDI - 4 )
    ^AB             \ STOSD                     ( RDI[0] = EAX ; RDI += 4 )
    ;

:C  ( R15 RSI:c RDI -- R15 RSI+ RDI+5 RAX~ ) \ "call"
    \ Compile a call to the label given next in source.
    ^53             \ PUSH RBX
    ^B0E8           \ MOV AL, E8        ( note: E8 = JUMP NEAR )
    ^AA             \ STOSB
    ^E8 ''O         \ CALL '
    ^E8 'OO         \ CALL O
    ^58             \ POP RBX
    ;

:J  ( R15 RSI:c RDI -- R15 RSI+ RDI+5 )  \ "jump"
    \ Compile a jump to the label given next in source.
    ^53             \ PUSH RBX
    ^B0E9           \ MOV AL, E9        ( note: E9 = JUMP NEAR )
    ^AA             \ STOSB
    C'              \ CALL quote
    CO              \ CALL O
    ^58             \ POP RBX
    ;

:%  ( RSI:xx.. -- RSI++ RDX=xx.. )
    \ Parse a hex literal into RDX.
    \ A negative sign can be prepended to turn it negative.

    ^31D2           \ XOR RDX, RDX
    ^AC             \ LODSB
    ^3C2D           \ CMP AL, '-'
    ^750A           \ JNE +10
    ^E804000000     \ CALL +4
    ^48F7DA         \ NEG RDX
    ^C3             \ RET

    ^AC             \ LODSB

    ^2C30           \ SUB AL, '0'
    ^7216           \ JL +22

    ^3C10           \ CMP AL, 0x10
    ^720A           \ JB +10
    ^3C11           \ CMP AL, 0x11
    ^720E           \ JB +14
    ^2C07           \ SUB AL, 0x07
    ^3C10           \ CMP AL, 0x10
    ^7308           \ JAE +8

    ^48C1E204       \ SHL RDX, 4
    ^08C2           \ OR DL, AL
    ^EBE5           \ JMP -27       ( 27 = 0x1B, 256-27 = 0xE5 )

    ^48FFCE         \ DEC RSI
    ;

:#  ( RSI:xx.. RDI DX=reg -- RSI++ RDI+9 DX ) ( RT: -- reg:xx )
    \ Compile a quadword hex literal to store in reg.
    \ The literal is parsed with %, so a
    \ Usage: %3#0001  \ puts the value 0x100 into register 3, i.e. RBX

    ^8AC2           \ MOV AL, DL
    ^C0E003         \ SHR AL, 3
    ^2401           \ AND AL, 0x01
    ^0C48           \ OR AL, 0x48
    ^AA             \ STOSB

    ^8AC2           \ MOV AL, DL
    ^2407           \ AND AL, 0x07
    ^0CB8           \ OR AL, 0xB8
    ^AA             \ STOSB

    C%              \ CALL %
    ^488BC2         \ MOV RAX, RDX
    ^48AB           \ STOSQ
    ;

:X
    \ ^4881E4FFFFFFF0 \ AND RSP, -16
    %0#2000001      \ MOV RAX, 0x2000001
    %7#-1           \ MOV RDI, 0x10
    ^0F05           \ SYSCALL
    ;

:]  ( R15 -- R15+800 )
    \ Enter compile mode. This shifts the dictionary up so we have
    \ access to a new set of labels.
    ^4981C700080000 \ ADD R15, 0x800
    ;

:[  ( R15+800 -- R15 )
    \ Exit compile mode. This shifts the dictionary down. Note that
    \ this definition should not be called in interpret mode!
    ^4981EF00080000 \ SUB R15, 0x800
    ;

:M  ( R15 RSI:c RDI -- R15[100+c]:RDI RSI+ RDI )  \ "macro"
    \ Begin a compile-time definition, ie. a macro. A macro is a
    \ word that is executed at compile time. This inserts the definition
    \ into the compile-time dictionary, instead of the default dictionary.
    C] C: J[

:c  ( R15+2048 RSI:c --  )
    \ This is the fallback action in compile mode. Similar to C but
    \ falls back to the default action of interpret mode if it can't
    \ find the appropriate action to in compile mode (presuming this
    \ will lead to an error handler).
    \ TODO Adapt this to emit a literal if the default handler returns
    \ a literal value. (Right now we're just assuming there are no
    \ literals, and the default handler never returns.)

    ^53             \ PUSH RBX
    C[              \ CALL [
    C'              \ CALL tick
    ^4885DB         \ TEST RBX, RBX
    ^7509           \ JNZ +9
      ^5B             \ POP RBX
      ^48FFCE         \ DEC RSI
      ^498B07         \ MOV RAX, [R15]
      ^FFE0           \ JMP RAX
    ^B0E8AA         \ MOV AL, E8; STOSB     ( note: E8 = CALL NEAR )
    C]              \ CALL ]
    CO              \ CALL O
    ^5B             \ POP RBX
    ;

:P ( RBX=c -- R15[100]=c )
    \ Prepare compile mode. This is run once to set up the "default"
    \ compile handler, which is c. [We briefly move into compile mode
    \ here just to have easier access to the compile mode dictionary.]
    C]
    ^49891F         \ MOV [R15], RBX
    C[
    ;

'c P

M[  J[          \]]
M;  C[ J;       \]
MC  JC
MJ  JJ
M\  J\
M(  J(          \))

:a;
:D  J:
:M  C] J:       \[
::  CD J]

\ Ok, now definitions (both regular and macro) will enter compile mode
\ automatically. So from now on we have to pay attention to that.

] ( this is a comment even in compile mode ) [

